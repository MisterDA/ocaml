/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Sylvain Le Gall for Lexifi                            */
/*                                                                        */
/*   Copyright 2008 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include "winworker.h"
#include <stdio.h>
#include <stdlib.h>
#include "windbug.h"
#include "winlist.h"

/* This constant define the maximum number of objects that
 * can be handle by a SELECTDATA.
 * It takes the following parameters into account:
 * - limitation on number of objects is mostly due to limitation
 *   a WaitForMultipleObjects
 * - there is always an event "hStop" to watch
 *
 * This lead to pick the following value as the biggest possible
 * value
 */
#define MAXIMUM_SELECT_OBJECTS (MAXIMUM_WAIT_OBJECTS - 1)

/* Manage set of handle */
typedef struct _SELECTHANDLESET {
  LPHANDLE lpHdl;
  DWORD    nMax;
  DWORD    nLast;
} SELECTHANDLESET;

typedef SELECTHANDLESET *LPSELECTHANDLESET;

static void handle_set_init (LPSELECTHANDLESET hds, LPHANDLE lpHdl, DWORD max)
{
  hds->lpHdl = lpHdl;
  hds->nMax  = max;
  hds->nLast = 0;

  /* Set to invalid value every entry of the handle */
  for (DWORD i = 0; i < hds->nMax; i++)
  {
    hds->lpHdl[i] = INVALID_HANDLE_VALUE;
  };
}

static void handle_set_add (LPSELECTHANDLESET hds, HANDLE hdl)
{
  if (hds->nLast < hds->nMax)
  {
    hds->lpHdl[hds->nLast] = hdl;
    hds->nLast++;
  }

  DEBUG_PRINT("Adding handle %x to set %x", hdl, hds);
}

static BOOL handle_set_mem (LPSELECTHANDLESET hds, HANDLE hdl)
{
  BOOL  res;

  res = FALSE;
  for (DWORD i = 0; !res && i < hds->nLast; i++)
  {
    res = (hds->lpHdl[i] == hdl);
  }

  return res;
}

static void handle_set_reset (LPSELECTHANDLESET hds)
{
  for (DWORD i = 0; i < hds->nMax; i++)
  {
    hds->lpHdl[i] = INVALID_HANDLE_VALUE;
  }
  hds->nMax  = 0;
  hds->nLast = 0;
  hds->lpHdl = NULL;
}

/* Data structure for handling select */

typedef enum _SELECTHANDLETYPE {
  SELECT_HANDLE_NONE = 0,
  SELECT_HANDLE_DISK,
  SELECT_HANDLE_CONSOLE,
  SELECT_HANDLE_PIPE,
  SELECT_HANDLE_SOCKET,
} SELECTHANDLETYPE;

typedef enum _SELECTMODE {
  SELECT_MODE_NONE = 0,
  SELECT_MODE_READ = 1,
  SELECT_MODE_WRITE = 2,
  SELECT_MODE_EXCEPT = 4,
} SELECTMODE;

typedef enum _SELECTSTATE {
  SELECT_STATE_NONE = 0,
  SELECT_STATE_INITFAILED,
  SELECT_STATE_ERROR,
  SELECT_STATE_SIGNALED
} SELECTSTATE;

typedef enum _SELECTTYPE {
  SELECT_TYPE_NONE = 0,
  SELECT_TYPE_STATIC,       /* Result is known without running anything */
  SELECT_TYPE_CONSOLE_READ, /* Reading data on console */
  SELECT_TYPE_PIPE_READ,    /* Reading data on pipe */
  SELECT_TYPE_SOCKET        /* Classic select */
} SELECTTYPE;

/* Data structure for query */
typedef struct _SELECTQUERY {
  LIST         lst;
  SELECTMODE   EMode;
  HANDLE       hFileDescr;
  unsigned int uFlagsFd; /* Copy of filedescr->flags_fd */
} SELECTQUERY;

typedef SELECTQUERY *LPSELECTQUERY;

typedef struct _SELECTDATA {
  LIST             lst;
  SELECTTYPE       EType;
  /* Sockets may generate results for all three lists from one query. */
  SELECTHANDLESET  readResults;
  HANDLE           aReadResults[MAXIMUM_SELECT_OBJECTS];
  SELECTHANDLESET  writeResults;
  HANDLE           aWriteResults[MAXIMUM_SELECT_OBJECTS];
  SELECTHANDLESET  exceptResults;
  HANDLE           aExceptResults[MAXIMUM_SELECT_OBJECTS];
  /* Data following are dedicated to APC like call, they
     will be initialized if required.
     */
  WORKERFUNC       funcWorker;
  SELECTQUERY      aQueries[MAXIMUM_SELECT_OBJECTS];
  DWORD            nQueriesCount;
  SELECTSTATE      EState;
  DWORD            nError;
  LPWORKER         lpWorker;
} SELECTDATA;

typedef SELECTDATA *LPSELECTDATA;

/* Get error status if associated condition is true. Only the first error is
   recorded. */
static BOOL check_error(LPSELECTDATA lpSelectData, BOOL bFailed)
{
  if (bFailed && lpSelectData->EState != SELECT_STATE_ERROR)
  {
    lpSelectData->EState = SELECT_STATE_ERROR;
    lpSelectData->nError = GetLastError();
    /* A failure must never be mistaken for a success */
    if (lpSelectData->nError == 0)
    {
      lpSelectData->nError = ERROR_GEN_FAILURE;
    }
  }
  return bFailed;
}

/* Create data associated with a  select operation */
static LPSELECTDATA select_data_new (LPSELECTDATA lpSelectData,
                                     SELECTTYPE EType)
{
  /* Allocate the data structure */
  LPSELECTDATA res;

  res = (LPSELECTDATA)caml_stat_alloc(sizeof(SELECTDATA));

  /* Init common data */
  caml_win32_list_init((LPLIST)res);
  caml_win32_list_next_set((LPLIST)res, (LPLIST)lpSelectData);
  res->EType         = EType;
  handle_set_init(&res->readResults, res->aReadResults,
                  MAXIMUM_SELECT_OBJECTS);
  handle_set_init(&res->writeResults, res->aWriteResults,
                  MAXIMUM_SELECT_OBJECTS);
  handle_set_init(&res->exceptResults, res->aExceptResults,
                  MAXIMUM_SELECT_OBJECTS);


  /* Data following are dedicated to APC like call, they
     will be initialized if required. For now they are set to
     invalid values.
     */
  res->funcWorker    = NULL;
  res->nQueriesCount = 0;
  res->EState        = SELECT_STATE_NONE;
  res->nError        = 0;
  res->lpWorker  = NULL;

  return res;
}

/* Free select data */
static void select_data_free (LPSELECTDATA lpSelectData)
{
  DEBUG_PRINT("Freeing data of %x", lpSelectData);

  /* Free APC related data, if they exists */
  if (lpSelectData->lpWorker != NULL)
  {
    caml_win32_worker_job_finish(lpSelectData->lpWorker);
    lpSelectData->lpWorker = NULL;
  };

  /* Make sure queries cannot be accessed */
  lpSelectData->nQueriesCount = 0;

  caml_stat_free(lpSelectData);
}

/* Add a result to select data. */
static void select_data_result_add (LPSELECTDATA lpSelectData,
                                    SELECTMODE EMode, HANDLE hFileDescr)
{
  switch (EMode)
  {
    case SELECT_MODE_READ:
      handle_set_add(&lpSelectData->readResults, hFileDescr);
      break;
    case SELECT_MODE_WRITE:
      handle_set_add(&lpSelectData->writeResults, hFileDescr);
      break;
    case SELECT_MODE_EXCEPT:
      handle_set_add(&lpSelectData->exceptResults, hFileDescr);
      break;
    case SELECT_MODE_NONE:
      CAMLunreachable();
  }
}

/* Add a query to select data, return zero if something goes wrong */
static DWORD select_data_query_add (LPSELECTDATA lpSelectData,
                                    SELECTMODE EMode,
                                    HANDLE hFileDescr,
                                    unsigned int uFlagsFd)
{
  DWORD res;
  DWORD i;

  res = 0;
  if (lpSelectData->nQueriesCount < MAXIMUM_SELECT_OBJECTS)
  {
    i = lpSelectData->nQueriesCount;
    lpSelectData->aQueries[i].EMode      = EMode;
    lpSelectData->aQueries[i].hFileDescr = hFileDescr;
    lpSelectData->aQueries[i].uFlagsFd   = uFlagsFd;
    lpSelectData->nQueriesCount++;
    res = 1;
  }

  return res;
}

/* Search for a job that has available query slots and that match provided type.
 * If none is found, create a new one. Return the corresponding SELECTDATA, and
 * update provided SELECTDATA head, if required.
 */
static LPSELECTDATA select_data_job_search (LPSELECTDATA *lppSelectData,
                                            SELECTTYPE EType)
{
  LPSELECTDATA res;

  res = NULL;

  /* Search for job */
  DEBUG_PRINT("Searching an available job for type %d", EType);
  res = *lppSelectData;
  while (
      res != NULL
      && !(
        res->EType == EType
        && res->nQueriesCount < MAXIMUM_SELECT_OBJECTS
        )
      )
  {
    res = LIST_NEXT(LPSELECTDATA, res);
  }

  /* No matching job found, create one */
  if (res == NULL)
  {
    DEBUG_PRINT("No job for type %d found, create one", EType);
    res = select_data_new(*lppSelectData, EType);
    *lppSelectData = res;
  }

  return res;
}

/***********************/
/*      Console        */
/***********************/

/* State of a console input buffer */
typedef enum _CONSOLESTATUS {
  CONSOLE_STATUS_ERROR = 0,
  CONSOLE_STATUS_EMPTY,     /* No input event */
  CONSOLE_STATUS_PENDING,   /* Input events, but reading would still block */
  CONSOLE_STATUS_READY      /* Reading would not block */
} CONSOLESTATUS;

/* A key event that a read may take part in: a key press, or the release of
 * Alt carrying a character that was pasted or composed with Alt+Numpad */
static BOOL console_record_is_key(const INPUT_RECORD *lpRecord)
{
  return lpRecord->EventType == KEY_EVENT
    && (lpRecord->Event.KeyEvent.bKeyDown
        || (lpRecord->Event.KeyEvent.wVirtualKeyCode == VK_MENU
            && lpRecord->Event.KeyEvent.uChar.UnicodeChar != 0));
}

static BOOL console_record_is_char(const INPUT_RECORD *lpRecord)
{
  return console_record_is_key(lpRecord)
    && lpRecord->Event.KeyEvent.uChar.UnicodeChar != 0;
}

/* Inspect the input buffer of a console to find out whether a read would
 * block.
 * - In line input mode, a read only returns once a whole line has been
 *   entered. Every key press may take part in line editing (arrows, history,
 *   etc.), so only the events that are not key presses can be discarded.
 * - Otherwise, a read returns as soon as a character is available and ignores
 *   every other event, which can all be discarded.
 * Ignored events are discarded so that they do not keep the console handle
 * signaled.
 */
static CONSOLESTATUS read_console_status(HANDLE hConsole)
{
  DWORD         mode;
  DWORD         nAvail;
  DWORD         n;
  DWORD         nDiscard;
  DWORD         err;
  BOOL          bLineInput;
  BOOL          bOk;
  PINPUT_RECORD lpRecords;
  CONSOLESTATUS res;

  if (!GetConsoleMode(hConsole, &mode))
  {
    return CONSOLE_STATUS_ERROR;
  }
  bLineInput = (mode & ENABLE_LINE_INPUT) != 0;

  while (TRUE)
  {
    if (!GetNumberOfConsoleInputEvents(hConsole, &nAvail))
    {
      return CONSOLE_STATUS_ERROR;
    }
    if (nAvail == 0)
    {
      return CONSOLE_STATUS_EMPTY;
    }

    /* This runs outside of the OCaml runtime: do not use caml_stat_alloc */
    lpRecords = (PINPUT_RECORD)malloc(sizeof(INPUT_RECORD) * nAvail);
    if (lpRecords == NULL)
    {
      SetLastError(ERROR_NOT_ENOUGH_MEMORY);
      return CONSOLE_STATUS_ERROR;
    }

    n = 0;
    nDiscard = 0;
    bOk = PeekConsoleInputW(hConsole, lpRecords, nAvail, &n);
    res = (n == 0 ? CONSOLE_STATUS_EMPTY : CONSOLE_STATUS_PENDING);
    for (DWORD i = 0; bOk && i < n && res != CONSOLE_STATUS_READY; i++)
    {
      if (console_record_is_char(&lpRecords[i])
          && (!bLineInput
              || lpRecords[i].Event.KeyEvent.uChar.UnicodeChar == L'\r'))
      {
        res = CONSOLE_STATUS_READY;
      }
    }

    if (bOk && res == CONSOLE_STATUS_PENDING)
    {
      /* Count the leading events that a read would ignore */
      while (nDiscard < n
             && (bLineInput
                 ? !console_record_is_key(&lpRecords[nDiscard])
                 : !console_record_is_char(&lpRecords[nDiscard])))
      {
        nDiscard++;
      }
      if (nDiscard > 0)
      {
        bOk = ReadConsoleInputW(hConsole, lpRecords, nDiscard, &n);
      }
    }

    err = GetLastError();
    free(lpRecords);
    SetLastError(err);

    if (!bOk)
    {
      return CONSOLE_STATUS_ERROR;
    }
    if (nDiscard == 0)
    {
      return res;
    }
    /* Some events were discarded, look at what remains */
  }
}

static void read_console_poll(HANDLE hStop, void *_data)
{
  HANDLE events[2];
  DWORD waitRes;
  BOOL bStopped;
  LPSELECTDATA  lpSelectData;
  LPSELECTQUERY lpQuery;

  DEBUG_PRINT("Waiting for data on console");

  lpSelectData = (LPSELECTDATA)_data;
  lpQuery = &(lpSelectData->aQueries[0]);

  /* WaitForMultipleObjects reports the signaled handle with the lowest index:
     put the console first, so that a stop request sent right away (when there
     are static results) does not hide input that is already available. */
  events[0] = lpQuery->hFileDescr;
  events[1] = hStop;
  while (lpSelectData->EState == SELECT_STATE_NONE)
  {
    switch (read_console_status(lpQuery->hFileDescr))
    {
      case CONSOLE_STATUS_ERROR:
        check_error(lpSelectData, TRUE);
        return;

      case CONSOLE_STATUS_READY:
        select_data_result_add(lpSelectData, lpQuery->EMode,
                               lpQuery->hFileDescr);
        lpSelectData->EState = SELECT_STATE_SIGNALED;
        return;

      case CONSOLE_STATUS_PENDING:
        /* The console handle stays signaled as long as there are events in
           the input buffer, so waiting for it would not block. Poll. */
        waitRes = WaitForSingleObject(hStop, 10);
        bStopped = (waitRes == WAIT_OBJECT_0);
        break;

      case CONSOLE_STATUS_EMPTY:
      default:
        waitRes = WaitForMultipleObjects(2, events, FALSE, INFINITE);
        bStopped = (waitRes == WAIT_OBJECT_0 + 1);
        break;
    }

    if (bStopped || check_error(lpSelectData, waitRes == WAIT_FAILED))
    {
      /* stop worker event or error */
      break;
    }
  }
}

/* Add a function to monitor console input */
static LPSELECTDATA read_console_poll_add (LPSELECTDATA lpSelectData,
                                           SELECTMODE EMode,
                                           HANDLE hFileDescr,
                                           unsigned int uFlagsFd)
{
  LPSELECTDATA res;

  res = select_data_new(lpSelectData, SELECT_TYPE_CONSOLE_READ);
  res->funcWorker = read_console_poll;
  select_data_query_add(res, SELECT_MODE_READ, hFileDescr, uFlagsFd);

  return res;
}

/***********************/
/*        Pipe         */
/***********************/

/* Monitor a pipe for input */
static void read_pipe_poll (HANDLE hStop, void *_data)
{
  DWORD         res;
  DWORD         event;
  DWORD         n;
  LPSELECTQUERY iterQuery;
  LPSELECTDATA  lpSelectData;
  DWORD         wait;

  /* Poll pipe */
  event = 0;
  n = 0;
  lpSelectData = (LPSELECTDATA)_data;
  wait = 1;

  DEBUG_PRINT("Checking data pipe");
  while (lpSelectData->EState == SELECT_STATE_NONE)
  {
    for (DWORD i = 0; i < lpSelectData->nQueriesCount; i++)
    {
      iterQuery = &(lpSelectData->aQueries[i]);
      res = PeekNamedPipe(
          iterQuery->hFileDescr,
          NULL,
          0,
          NULL,
          &n,
          NULL);
      if (check_error(lpSelectData,
            (res == 0) &&
            (GetLastError() != ERROR_BROKEN_PIPE)))
      {
        break;
      };

      if ((n > 0) || (res == 0))
      {
        lpSelectData->EState = SELECT_STATE_SIGNALED;
        select_data_result_add(lpSelectData, iterQuery->EMode,
                               iterQuery->hFileDescr);
      };
    };

    /* Alas, nothing except polling seems to work for pipes.
       Check the state & stop_worker_event every 10 ms
     */
    if (lpSelectData->EState == SELECT_STATE_NONE)
    {
      event = WaitForSingleObject(hStop, wait);

      /* Fast start: begin to wait 1, 2, 4, 8 and then 10 ms.
       * If we are working with the output of a program there is
       * a chance that one of the 4 first calls succeed.
       */
      wait = 2 * wait;
      if (wait > 10)
      {
        wait = 10;
      };
      if (event == WAIT_OBJECT_0
          || check_error(lpSelectData, event == WAIT_FAILED))
      {
        break;
      }
    }
  }
  DEBUG_PRINT("Finish checking data on pipe");
}

/* Add a function to monitor pipe input */
static LPSELECTDATA read_pipe_poll_add (LPSELECTDATA lpSelectData,
                                        SELECTMODE EMode,
                                        HANDLE hFileDescr,
                                        unsigned int uFlagsFd)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;

  hd = lpSelectData;
  /* Polling pipe is a non blocking operation by default. This means that each
     worker can handle many pipe. We begin to try to find a worker that is
     polling pipe, but for which there is under the limit of pipe per worker.
     */
  DEBUG_PRINT("Searching an available worker handling pipe");
  res = select_data_job_search(&hd, SELECT_TYPE_PIPE_READ);

  /* Add a new pipe to poll */
  res->funcWorker = read_pipe_poll;
  select_data_query_add(res, EMode, hFileDescr, uFlagsFd);

  return hd;
}

/***********************/
/*       Socket        */
/***********************/

/* Collect the results of the queries whose event is signaled. Return the
   number of results that were added. */
static DWORD socket_poll_results (LPSELECTDATA lpSelectData, HANDLE *aEvents)
{
  LPSELECTQUERY    iterQuery;
  WSANETWORKEVENTS events;
  BOOL             bConnectFailed;
  DWORD            nResults;

  nResults = 0;
  for (DWORD i = 0; i < lpSelectData->nQueriesCount; i++)
  {
    iterQuery = &(lpSelectData->aQueries[i]);
    if (WaitForSingleObject(aEvents[i], 0) != WAIT_OBJECT_0)
    {
      continue;
    }

    DEBUG_PRINT("Socket %d has pending events", i);
    /* Find out what kind of events were raised. This also resets the event
       object, and each network event is recorded again only once it has been
       re-enabled (e.g. by a call to recv for FD_READ). */
    if (check_error(lpSelectData,
          WSAEnumNetworkEvents((SOCKET)(iterQuery->hFileDescr),
                               aEvents[i], &events) != 0))
    {
      break;
    }

    /* FD_CONNECT is also raised when the socket is already connected at the
       time of WSAEventSelect: only a failed connection attempt is a result. A
       successful one raises FD_WRITE. */
    bConnectFailed = (events.lNetworkEvents & FD_CONNECT) != 0
                     && events.iErrorCode[FD_CONNECT_BIT] != 0;

    if ((iterQuery->EMode & SELECT_MODE_READ) != 0
        && (events.lNetworkEvents & (FD_READ | FD_ACCEPT | FD_CLOSE)) != 0)
    {
      select_data_result_add(lpSelectData, SELECT_MODE_READ,
                             iterQuery->hFileDescr);
      nResults++;
    }
    /* Report a failed connection as writable like POSIX does... */
    if ((iterQuery->EMode & SELECT_MODE_WRITE) != 0
        && ((events.lNetworkEvents & (FD_WRITE | FD_CLOSE)) != 0
            || bConnectFailed))
    {
      select_data_result_add(lpSelectData, SELECT_MODE_WRITE,
                             iterQuery->hFileDescr);
      nResults++;
    }
    /* ... and as exceptional like Winsock's select does. */
    if ((iterQuery->EMode & SELECT_MODE_EXCEPT) != 0
        && ((events.lNetworkEvents & FD_OOB) != 0 || bConnectFailed))
    {
      select_data_result_add(lpSelectData, SELECT_MODE_EXCEPT,
                             iterQuery->hFileDescr);
      nResults++;
    }
  }

  return nResults;
}

/* Monitor socket */
static void socket_poll (HANDLE hStop, void *_data)
{
  LPSELECTDATA   lpSelectData;
  LPSELECTQUERY    iterQuery;
  /* One event per query, plus hStop */
  HANDLE           aEvents[MAXIMUM_SELECT_OBJECTS + 1];
  DWORD            nEvents;
  DWORD            waitRes;
  long             maskEvents;
  u_long           iMode;
  SELECTMODE       mode;

  lpSelectData = (LPSELECTDATA)_data;

  DEBUG_PRINT("Worker has %d queries to service", lpSelectData->nQueriesCount);
  for (nEvents = 0; nEvents < lpSelectData->nQueriesCount; nEvents++)
  {
    iterQuery = &(lpSelectData->aQueries[nEvents]);
    aEvents[nEvents] = CreateEvent(NULL, TRUE, FALSE, NULL);
    check_error(lpSelectData, aEvents[nEvents] == NULL);
    maskEvents = 0;
    mode = iterQuery->EMode;
    if ((mode & SELECT_MODE_READ) != 0)
    {
      DEBUG_PRINT("Polling read for %d", iterQuery->hFileDescr);
      maskEvents |= FD_READ | FD_ACCEPT | FD_CLOSE;
    }
    if ((mode & SELECT_MODE_WRITE) != 0)
    {
      DEBUG_PRINT("Polling write for %d", iterQuery->hFileDescr);
      maskEvents |= FD_WRITE | FD_CONNECT | FD_CLOSE;
    }
    if ((mode & SELECT_MODE_EXCEPT) != 0)
    {
      DEBUG_PRINT("Polling exceptions for %d", iterQuery->hFileDescr);
      maskEvents |= FD_OOB | FD_CONNECT;
    }

    check_error(lpSelectData,
        WSAEventSelect(
          (SOCKET)(iterQuery->hFileDescr),
          aEvents[nEvents],
          maskEvents) == SOCKET_ERROR);
  }

  /* Add stop event */
  aEvents[nEvents]  = hStop;
  nEvents++;

  /* Some network events do not yield any result (see socket_poll_results):
     keep waiting until there is a result, an error, or a stop request. */
  while (lpSelectData->nError == 0)
  {
    waitRes = WaitForMultipleObjects(nEvents, aEvents, FALSE, INFINITE);
    if (check_error(lpSelectData, waitRes == WAIT_FAILED))
    {
      break;
    }
    if (socket_poll_results(lpSelectData, aEvents) > 0
        || waitRes == WAIT_OBJECT_0 + nEvents - 1)
    {
      break;
    }
  }

  /* The events and the WSAEventSelect associations must be released in every
     case. */
  for (DWORD i = 0; i < lpSelectData->nQueriesCount; i++)
  {
    iterQuery = &(lpSelectData->aQueries[i]);

    if (aEvents[i] == NULL)
    {
      continue;
    }

    /* WSAEventSelect() automatically sets socket to nonblocking mode.
       Restore the blocking one. */
    if (iterQuery->uFlagsFd & FLAGS_FD_IS_BLOCKING)
    {
      DEBUG_PRINT("Restore a blocking socket");
      iMode = 0;
      check_error(lpSelectData,
        WSAEventSelect((SOCKET)(iterQuery->hFileDescr), aEvents[i], 0) != 0 ||
        ioctlsocket((SOCKET)(iterQuery->hFileDescr), FIONBIO, &iMode) != 0);
    }
    else
    {
      check_error(lpSelectData,
        WSAEventSelect((SOCKET)(iterQuery->hFileDescr), aEvents[i], 0) != 0);
    };

    CloseHandle(aEvents[i]);
    aEvents[i] = INVALID_HANDLE_VALUE;
  }
}

/* Add a function to monitor socket */
static LPSELECTDATA socket_poll_add (LPSELECTDATA lpSelectData,
                                     SELECTMODE EMode,
                                     HANDLE hFileDescr,
                                     unsigned int uFlagsFd)
{
  LPSELECTDATA res;
  LPSELECTDATA candidate;
  long i;
  LPSELECTQUERY aQueries;

  res = lpSelectData;
  candidate = NULL;
  aQueries = NULL;

  /* Polling socket can be done multiple handle at the same time. You just
     need one worker to use it. Try to find if there is already a worker
     handling this kind of request.
     Only one event can be associated with a given socket which means
     that if a socket is in more than one of the fd_sets then we have
     to find that particular query and update EMode with the
     additional flag.
     */
  DEBUG_PRINT("Scanning list of worker to find one that already handle socket");
  /* Search for job */
  DEBUG_PRINT("Searching for an available job for type %d for descriptor %d",
              SELECT_TYPE_SOCKET, hFileDescr);
  while (res != NULL)
  {
    if (res->EType == SELECT_TYPE_SOCKET)
    {
      i = res->nQueriesCount - 1;
      aQueries = res->aQueries;
      while (i >= 0 && aQueries[i].hFileDescr != hFileDescr)
      {
        i--;
      }
      /* If we didn't find the socket but this worker has available
         slots, store it
       */
      if (i < 0)
      {
        if ( res->nQueriesCount < MAXIMUM_SELECT_OBJECTS)
        {
          candidate = res;
        }
        res = LIST_NEXT(LPSELECTDATA, res);
      }
      else
      {
        /* Previous socket query located -- we're finished
         */
        aQueries = &aQueries[i];
        break;
      }
    }
    else
    {
      res = LIST_NEXT(LPSELECTDATA, res);
    }
  }

  if (res == NULL)
  {
    res = candidate;

    /* No matching job found, create one */
    if (res == NULL)
    {
      DEBUG_PRINT("No job for type %d found, create one", SELECT_TYPE_SOCKET);
      lpSelectData = res = select_data_new(lpSelectData, SELECT_TYPE_SOCKET);
      res->funcWorker = socket_poll;
      res->nQueriesCount = 1;
      aQueries = &res->aQueries[0];
    }
    else
    {
      aQueries = &(res->aQueries[res->nQueriesCount++]);
    }
    aQueries->EMode = EMode;
    aQueries->hFileDescr = hFileDescr;
    aQueries->uFlagsFd = uFlagsFd;
    DEBUG_PRINT("Socket %x added", hFileDescr);
  }
  else
  {
    aQueries->EMode |= EMode;
    DEBUG_PRINT("Socket %x updated to %d", hFileDescr, aQueries->EMode);
  }

  return lpSelectData;
}

/***********************/
/*       Static        */
/***********************/

/* Add a static result */
static LPSELECTDATA static_poll_add (LPSELECTDATA lpSelectData,
                                     SELECTMODE EMode,
                                     HANDLE hFileDescr,
                                     unsigned int uFlagsFd)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;

  /* Look for an already initialized static element */
  hd = lpSelectData;
  res = select_data_job_search(&hd, SELECT_TYPE_STATIC);

  /* Add a new query/result */
  select_data_query_add(res, EMode, hFileDescr, uFlagsFd);
  select_data_result_add(res, EMode, hFileDescr);

  return hd;
}

/********************************/
/* Generic select data handling */
/********************************/

/* Guess handle type */
static SELECTHANDLETYPE get_handle_type(value fd)
{
  DWORD            mode;
  SELECTHANDLETYPE res;

  CAMLparam1(fd);

  mode = 0;
  res = SELECT_HANDLE_NONE;

  if (Descr_kind_val(fd) == KIND_SOCKET)
  {
    res = SELECT_HANDLE_SOCKET;
  }
  else
  {
    switch(GetFileType(Handle_val(fd)))
    {
      case FILE_TYPE_DISK:
        res = SELECT_HANDLE_DISK;
        break;

      case FILE_TYPE_CHAR: /* character file or a console */
        if (GetConsoleMode(Handle_val(fd), &mode) != 0)
        {
          res = SELECT_HANDLE_CONSOLE;
        }
        else
        {
          res = SELECT_HANDLE_NONE;
        };
        break;

      case FILE_TYPE_PIPE: /* a named or an anonymous pipe (socket
                              already handled) */
        res = SELECT_HANDLE_PIPE;
        break;
    };
  };

  CAMLreturnT(SELECTHANDLETYPE, res);
}

/* Choose what to do with given data. Return FALSE if the handle cannot be
   selected on. */
static BOOL select_data_dispatch (LPSELECTDATA *lppSelectData,
                                  SELECTMODE EMode,
                                  value fd)
{
  LPSELECTDATA    res;
  BOOL            bSupported;
  HANDLE          hFileDescr;
  struct sockaddr sa;
  int             sa_len;
  BOOL            alreadyAdded;
  unsigned int    uFlagsFd;

  CAMLparam1(fd);

  res          = *lppSelectData;
  bSupported   = TRUE;
  hFileDescr   = Handle_val(fd);
  sa_len       = sizeof(sa);
  alreadyAdded = FALSE;
  uFlagsFd     = Flags_fd_val(fd);

  DEBUG_PRINT("Begin dispatching handle %x", hFileDescr);

  DEBUG_PRINT("Waiting for %d on handle %x", EMode, hFileDescr);

  /* There is only 2 way to have except mode: transmission of OOB data through
     a socket TCP/IP and through a strange interaction with a TTY.
     With windows, we only consider the TCP/IP except condition
  */
  switch(get_handle_type(fd))
  {
    case SELECT_HANDLE_DISK:
      DEBUG_PRINT("Handle %x is a disk handle", hFileDescr);
      /* Disk is always ready in read/write operation */
      if (EMode == SELECT_MODE_READ || EMode == SELECT_MODE_WRITE)
      {
        res = static_poll_add(res, EMode, hFileDescr, uFlagsFd);
      };
      break;

    case SELECT_HANDLE_CONSOLE:
      DEBUG_PRINT("Handle %x is a console handle", hFileDescr);
      /* Console is always ready in write operation, need to check for read. */
      if (EMode == SELECT_MODE_READ)
      {
        res = read_console_poll_add(res, EMode, hFileDescr, uFlagsFd);
      }
      else if (EMode == SELECT_MODE_WRITE)
      {
        res = static_poll_add(res, EMode, hFileDescr, uFlagsFd);
      };
      break;

    case SELECT_HANDLE_PIPE:
      DEBUG_PRINT("Handle %x is a pipe handle", hFileDescr);
      /* Console is always ready in write operation, need to check for read. */
      if (EMode == SELECT_MODE_READ)
      {
        DEBUG_PRINT("Need to check availability of data on pipe");
        res = read_pipe_poll_add(res, EMode, hFileDescr, uFlagsFd);
      }
      else if (EMode == SELECT_MODE_WRITE)
      {
        DEBUG_PRINT("No need to check availability of data on pipe, "
                    "write operation always possible");
        res = static_poll_add(res, EMode, hFileDescr, uFlagsFd);
      };
      break;

    case SELECT_HANDLE_SOCKET:
      DEBUG_PRINT("Handle %x is a socket handle", hFileDescr);
      if (getsockname((SOCKET)hFileDescr, &sa, &sa_len) == SOCKET_ERROR)
      {
        if (WSAGetLastError() == WSAEINVAL)
        {
          /* Socket is not bound */
          DEBUG_PRINT("Socket is not connected");
          if (EMode == SELECT_MODE_WRITE || EMode == SELECT_MODE_READ)
          {
            res = static_poll_add(res, EMode, hFileDescr, uFlagsFd);
            alreadyAdded = TRUE;
          }
        }
      }
      if (!alreadyAdded)
      {
        res = socket_poll_add(res, EMode, hFileDescr, uFlagsFd);
      }
      break;

    default:
      DEBUG_PRINT("Handle %x is unknown", hFileDescr);
      bSupported = FALSE;
      break;
  };

  DEBUG_PRINT("Finish dispatching handle %x", hFileDescr);

  *lppSelectData = res;
  CAMLreturnT(BOOL, bSupported);
}

/* Dispatch every descriptor of a list, ignoring duplicates. hds is used as
   scratch space. Return FALSE if a handle cannot be selected on. */
static BOOL select_data_dispatch_list (LPSELECTDATA *lppSelectData,
                                       LPSELECTHANDLESET hds,
                                       LPHANDLE hdsData, DWORD hdsMax,
                                       value fdlist, SELECTMODE EMode)
{
  BOOL res;

  CAMLparam1(fdlist);
  CAMLlocal1(fd);

  res = TRUE;
  handle_set_init(hds, hdsData, hdsMax);
  for (; res && fdlist != Val_emptylist; fdlist = Field(fdlist, 1))
  {
    fd = Field(fdlist, 0);
    if (!handle_set_mem(hds, Handle_val(fd)))
    {
      handle_set_add(hds, Handle_val(fd));
      res = select_data_dispatch(lppSelectData, EMode, fd);
    }
    else
    {
      DEBUG_PRINT("Discarding handle %x which is already monitored "
                  "for mode %d", Handle_val(fd), EMode);
    }
  }
  handle_set_reset(hds);

  CAMLreturnT(BOOL, res);
}

static DWORD caml_list_length (value lst)
{
  DWORD res;

  CAMLparam1 (lst);
  CAMLlocal1 (l);

  for (res = 0, l = lst; l != Val_emptylist; l = Field(l, 1), res++)
  { }

  CAMLreturnT(DWORD, res);
}

static LPSELECTHANDLESET select_data_results(LPSELECTDATA lpSelectData,
                                             SELECTMODE EMode)
{
  switch (EMode)
  {
    case SELECT_MODE_READ:
      return &lpSelectData->readResults;
    case SELECT_MODE_WRITE:
      return &lpSelectData->writeResults;
    case SELECT_MODE_EXCEPT:
      return &lpSelectData->exceptResults;
    case SELECT_MODE_NONE:
      CAMLunreachable();
  }
  return NULL; /* Avoid warning C4715 with MSVC. */
}

static BOOL select_data_result_mem(LPSELECTDATA lpSelectData,
                                   SELECTMODE EMode, HANDLE hFileDescr)
{
  for (; lpSelectData != NULL;
       lpSelectData = LIST_NEXT(LPSELECTDATA, lpSelectData))
  {
    if (handle_set_mem(select_data_results(lpSelectData, EMode), hFileDescr))
      return TRUE;
  }
  return FALSE;
}

/* Filter an original descriptor list using the results.  Iterating over the
   original list both returns the exact OCaml values supplied by the caller
   and preserves duplicate descriptors, as the fd_set implementation does. */
static value select_data_to_fdlist(value fdlist, LPSELECTDATA lpSelectData,
                                   SELECTMODE EMode)
{
  CAMLparam1(fdlist);
  CAMLlocal2(res, fd);

  res = Val_emptylist;
  for (; fdlist != Val_emptylist; fdlist = Field(fdlist, 1))
  {
    fd = Field(fdlist, 0);
    if (select_data_result_mem(lpSelectData, EMode, Handle_val(fd)))
    {
      value newres = caml_alloc_small(2, Tag_cons);
      Field(newres, 0) = fd;
      Field(newres, 1) = res;
      res = newres;
    }
  }

  CAMLreturn(res);
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))

/* Convert fdlist to an fd_set if all the handles in fdlist are
 * sockets and return 1.  Returns 0 if a non-socket value is
 * encountered, or if there are more than FD_SETSIZE sockets.
 */
static int fdlist_to_fdset(value fdlist, fd_set *fdset)
{
  value c;
  int n = 0;
  FD_ZERO(fdset);
  for (value l = fdlist; l != Val_emptylist; l = Field(l, 1)) {
    if (++n > FD_SETSIZE) {
      DEBUG_PRINT("More than FD_SETSIZE sockets");
      return 0;
    }
    c = Field(l, 0);
    if (Descr_kind_val(c) == KIND_SOCKET) {
      FD_SET(Socket_val(c), fdset);
    } else {
      DEBUG_PRINT("Non socket value encountered");
      return 0;
    }
  }
  return 1;
}

static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  CAMLparam1(fdlist);
  CAMLlocal2(res, s);
  res = Val_emptylist;
  for (/*nothing*/; fdlist != Val_emptylist; fdlist = Field(fdlist, 1)) {
    s = Field(fdlist, 0);
    if (FD_ISSET(Socket_val(s), fdset)) {
      value newres = caml_alloc_small(2, Tag_cons);
      Field(newres, 0) = s;
      Field(newres, 1) = res;
      res = newres;
    }
  }
  CAMLreturn(res);
}


/* Convert a timeout in seconds to milliseconds, a negative timeout being
   infinite */
static DWORD select_timeout_msec(double tm_sec)
{
  double msec;

  if (tm_sec < 0.0)
  {
    return INFINITE;
  }
  msec = tm_sec * MSEC_PER_SEC;
  return msec < INFINITE ? (DWORD) msec : INFINITE - 1;
}

CAMLprim value caml_unix_select(value readfds, value writefds, value exceptfds,
                                value timeout_sec)
{
  /* Event associated to handle */
  DWORD   nEventsCount;
  DWORD   nEventsMax;
  HANDLE *lpEventsDone;

  /* Data for all handles */
  LPSELECTDATA lpSelectData;
  LPSELECTDATA iterSelectData;

  /* Error status */
  DWORD err;

  /* Time to wait */
  DWORD tm_msec;

  /* Is there static select data */
  BOOL  hasStaticData = FALSE;

  /* Set of handle */
  SELECTHANDLESET hds;
  DWORD           hdsMax;
  LPHANDLE        hdsData;

  /* Length of each list */
  DWORD readfds_len;
  DWORD writefds_len;
  DWORD exceptfds_len;

  CAMLparam4 (readfds, writefds, exceptfds, timeout_sec);
  CAMLlocal4 (read_list, write_list, except_list, res);

  fd_set read, write, except;
  double tm_sec;
  struct timeval tv;
  struct timeval * tvp;

  DEBUG_PRINT("in select");

  err = 0;
  tm_sec = Double_val(timeout_sec);
  if (readfds == Val_emptylist
      && writefds == Val_emptylist
      && exceptfds == Val_emptylist) {
    DEBUG_PRINT("nothing to do");
    tm_msec = select_timeout_msec(tm_sec);
    if (tm_msec > 0) {
      caml_enter_blocking_section();
      Sleep(tm_msec);
      caml_leave_blocking_section();
    }
    read_list = write_list = except_list = Val_emptylist;
  } else {
    if (fdlist_to_fdset(readfds, &read)
        && fdlist_to_fdset(writefds, &write)
        && fdlist_to_fdset(exceptfds, &except)) {
      DEBUG_PRINT("only sockets to select on, using classic select");
      if (tm_sec < 0.0) {
        tvp = (struct timeval *) NULL;
      } else {
        tv = caml_timeval_of_sec(tm_sec);
        tvp = &tv;
      }
      caml_enter_blocking_section();
      if (select(FD_SETSIZE, &read, &write, &except, tvp) == -1) {
        err = WSAGetLastError();
        DEBUG_PRINT("Error %ld occurred", err);
      }
      caml_leave_blocking_section();
      if (err) {
        DEBUG_PRINT("Error %ld occurred", err);
        caml_win32_maperr(err);
        caml_uerror("select", Nothing);
      }
      read_list = fdset_to_fdlist(readfds, &read);
      write_list = fdset_to_fdlist(writefds, &write);
      except_list = fdset_to_fdlist(exceptfds, &except);
    } else {
      nEventsCount   = 0;
      nEventsMax     = 0;
      lpEventsDone   = NULL;
      lpSelectData   = NULL;
      iterSelectData = NULL;
      hasStaticData  = 0;
      readfds_len    = caml_list_length(readfds);
      writefds_len   = caml_list_length(writefds);
      exceptfds_len  = caml_list_length(exceptfds);
      hdsMax         = MAX(readfds_len, MAX(writefds_len, exceptfds_len));

      hdsData = (HANDLE *)caml_stat_alloc(sizeof(HANDLE) * hdsMax);

      tm_msec = select_timeout_msec(tm_sec);
      DEBUG_PRINT("Will wait %d ms", tm_msec);

      /* Create list of select data, based on the different list of fd
         to watch. Nothing may raise until the list is freed. */
      DEBUG_PRINT("Dispatch fds");
      if (!select_data_dispatch_list(&lpSelectData, &hds, hdsData, hdsMax,
                                     readfds, SELECT_MODE_READ)
          || !select_data_dispatch_list(&lpSelectData, &hds, hdsData, hdsMax,
                                        writefds, SELECT_MODE_WRITE)
          || !select_data_dispatch_list(&lpSelectData, &hds, hdsData, hdsMax,
                                        exceptfds, SELECT_MODE_EXCEPT))
        {
          err = ERROR_INVALID_HANDLE;
        }

      /* Count the workers to run: the main thread waits for all of them at
         once, which is limited to MAXIMUM_WAIT_OBJECTS handles. */
      for (iterSelectData = lpSelectData; iterSelectData != NULL;
           iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData))
        {
          if (iterSelectData->funcWorker != NULL)
            {
              nEventsMax++;
            }
        }
      if (err == 0 && nEventsMax > MAXIMUM_WAIT_OBJECTS)
        {
          DEBUG_PRINT("Too many workers required: %d", nEventsMax);
          err = ERROR_INVALID_PARAMETER;
        }

      if (err == 0)
        {
          /* Building the list of handle to wait for */
          DEBUG_PRINT("Building events done array");
          lpEventsDone =
            (HANDLE *)caml_stat_alloc_noexc(sizeof(HANDLE) * nEventsMax);
          if (lpEventsDone == NULL && nEventsMax > 0)
            {
              err = ERROR_NOT_ENOUGH_MEMORY;
            }
        }

      if (err == 0)
        {
          iterSelectData = lpSelectData;
          while (iterSelectData != NULL)
            {
              /* Check if it is static data. If this is the case, launch
               * everything but don't wait for events. It helps to test if
               * there are events on any other fd (which are not static),
               * knowing that there is at least one result (the static data).
               */
              if (iterSelectData->EType == SELECT_TYPE_STATIC)
                {
                  hasStaticData = TRUE;
                };

              /* Execute APC */
              if (iterSelectData->funcWorker != NULL)
                {
                  iterSelectData->lpWorker =
                    caml_win32_worker_job_submit(iterSelectData->funcWorker,
                                                 (void *)iterSelectData);
                  DEBUG_PRINT("Job submitted to worker %x",
                              iterSelectData->lpWorker);
                  lpEventsDone[nEventsCount]
                    = caml_win32_worker_job_event_done(
                        iterSelectData->lpWorker);
                  nEventsCount++;
                };
              iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
            };

          DEBUG_PRINT("Need to watch %d workers", nEventsCount);

          /* Processing select itself */
          caml_enter_blocking_section();
          /* There are worker started, waiting to be monitored */
          if (nEventsCount > 0)
            {
              /* Waiting for event */
              if (!hasStaticData)
                {
                  DEBUG_PRINT("Waiting for one select worker to be done");
                  switch (WaitForMultipleObjects(nEventsCount, lpEventsDone,
                                                 FALSE, tm_msec))
                    {
                    case WAIT_FAILED:
                      err = GetLastError();
                      break;

                    case WAIT_TIMEOUT:
                      DEBUG_PRINT("Select timeout");
                      break;

                    default:
                      DEBUG_PRINT("One worker is done");
                      break;
                    };
                }

              /* Ordering stop to every worker */
              DEBUG_PRINT("Sending stop signal to every select workers");
              iterSelectData = lpSelectData;
              while (iterSelectData != NULL)
                {
                  if (iterSelectData->lpWorker != NULL)
                    {
                      caml_win32_worker_job_stop(iterSelectData->lpWorker);
                    };
                  iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
                };

              DEBUG_PRINT("Waiting for every select worker to be done");
              switch (WaitForMultipleObjects(nEventsCount, lpEventsDone, TRUE,
                                             INFINITE))
                {
                case WAIT_FAILED:
                  err = GetLastError();
                  break;

                default:
                  DEBUG_PRINT("Every worker is done");
                  break;
                }
            }
          /* Nothing to monitor but some time to wait. */
          else if (!hasStaticData)
            {
              Sleep(tm_msec);
            }
          caml_leave_blocking_section();
        }

      DEBUG_PRINT("Error status: %d (0 is ok)", err);
      /* Build results */
      if (err == 0)
        {
          iterSelectData = lpSelectData;
          while (iterSelectData != NULL)
            {
              /* We try to only process the first error, bypass other errors */
              if (err == 0 && iterSelectData->EState == SELECT_STATE_ERROR)
                {
                  err = iterSelectData->nError;
                }
              iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
            }

          if (err == 0)
            {
              DEBUG_PRINT("Building result");
              read_list = select_data_to_fdlist(readfds, lpSelectData,
                                                SELECT_MODE_READ);
              write_list = select_data_to_fdlist(writefds, lpSelectData,
                                                 SELECT_MODE_WRITE);
              except_list = select_data_to_fdlist(exceptfds, lpSelectData,
                                                  SELECT_MODE_EXCEPT);
            }
        }

      /* Free resources */
      DEBUG_PRINT("Free selectdata resources");
      iterSelectData = lpSelectData;
      while (iterSelectData != NULL)
        {
          lpSelectData = iterSelectData;
          iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
          select_data_free(lpSelectData);
        }
      lpSelectData = NULL;

      /* Free allocated events/handle set array */
      DEBUG_PRINT("Free local allocated resources");
      caml_stat_free(lpEventsDone);
      caml_stat_free(hdsData);

      DEBUG_PRINT("Raise error if required");
      if (err != 0)
        {
          caml_win32_maperr(err);
          caml_uerror("select", Nothing);
        }
    }
  }

  DEBUG_PRINT("Build final result");
  res = caml_alloc_small(3, 0);
  Field(res, 0) = read_list;
  Field(res, 1) = write_list;
  Field(res, 2) = except_list;

  DEBUG_PRINT("out select");

  CAMLreturn(res);
}
