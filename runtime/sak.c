/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2021 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Runtime Builder's Swiss Army Knife. This utility performs functions
   previously delegated to classic Unix utilities but which ultimately seem to
   cause more hassle for maintenance than the initial simplicity suggests.

   This tool is a memorial to the many hours and PRs spent chasing down strange
   locale issues, stray CR characters and fighting yet another incompatible
   implementation of sed or awk. */

/* Borrow the Unicode *_os definitions and T() macro from misc.h */
#define CAML_INTERNALS
#include "caml/misc.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#if defined(_WIN32)
#include <direct.h>
#elif defined(HAS_UNISTD)
#include <unistd.h>
#endif

#ifdef _WIN32
#define fputs_os fputws
#define fprintf_os fwprintf
#define perror_os _wperror
#define fgets_os fgetws
#define strncmp_os wcsncmp
#define strchr_os wcschr
#else
#define fputs_os fputs
#define fprintf_os fprintf
#define perror_os perror
#define fgets_os fgets
#define strncmp_os strncmp
#define strchr_os strchr
#endif

/* Operations
   - encode-C-literal. Used for the OCAML_STDLIB_DIR macro in
     runtime/build_config.h to ensure the LIBDIR make variable is correctly
     represented as a C string literal.

     On Unix, `sak encode-C-literal /usr/local/lib` returns `"/usr/local/lib"`

     On Windows, `sak encode-C-literal "C:\OCaml🐫\lib"` returns
     `L"C:\\OCaml\xd83d\xdc2b\\lib"`
 */

static void usage(void)
{
  printf(
    "OCaml Build System Swiss Army Knife\n"
    "Usage: sak command\n"
    "Commands:\n"
    " * encode-C-literal path - encodes path as a C string literal\n"
    " * show-includes <target> <output> <cfile>"
      " - transforms MSVC /showInclude for Make inclusion\n"
    "   cl /nologo /Zs /showIncludes /c myfile.c | "
         "sak show-includes myfile.obj myfile.d myfile.c\n"
  );
}

/* Converts the supplied path (UTF-8 on Unix and UCS-2ish on Windows) to a valid
   C string literal. On Windows, this is always a wchar_t* (L"..."). */
static void encode_C_literal(const char_os *path)
{
  char_os c;

#ifdef _WIN32
  putchar('L');
#endif
  putchar('"');

  while ((c = *path++) != 0) {
    /* Escape \, " and \n */
    if (c == '\\') {
      printf("\\\\");
    } else if (c == '"') {
      printf("\\\"");
    } else if (c == '\n') {
      printf("\\n");
#ifndef _WIN32
    /* On Unix, nothing else needs escaping */
    } else {
      putchar(c);
#else
    /* On Windows, allow 7-bit printable characters to be displayed literally
       and escape everything else (using the older \x notation for increased
       compatibility, rather than the newer \U. */
    } else if (c < 0x80 && iswprint(c)) {
      putwchar(c);
    } else {
      printf("\\x%04x", c);
#endif
    }
  }

  putchar('"');
}

static bool filename_is_relative(const char_os *file)
{
  size_t len = strlen_os(file);
  return (len < 1 || file[0] != T('/'))
    && (len < 1 || file[0] != T('\\'))
    && (len < 2 || file[1] != T(':'));
}

static void show_includes(const char_os *target, const char_os *output,
                          const char_os *cfile)
{
  char_os note[] = T("Note: including file: ");
  char_os buf[sizeof(note) / sizeof(char_os) + 127 + 32767];
  int rc;

  FILE *out = fopen(output, "wb");
  if (out == NULL) {
    perror_os(T("fopen"));
    exit(EXIT_FAILURE);
  }

  char_os *cwd;
  if ((cwd = getcwd_os(NULL, 0)) == NULL) {
    perror_os(T("getcwd"));
    exit(EXIT_FAILURE);
  }
  size_t cwd_len = strlen_os(cwd);

  fprintf_os(out, T("%s: %s"), target, cfile);
  size_t columns = strlen_os(target) + 2 + strlen_os(cfile);

  /* skip the first line (file name) */
  if (fgets_os(buf, sizeof(buf) / sizeof(char_os), stdin) == NULL) {
    if (ferror(stdin)) {
      perror_os(T("fgetsws"));
      exit(EXIT_FAILURE);
    }
  }

  if (columns >= 78) {
    fputs_os(T(" \\\n"), out);
    columns = 0;
  }

  while (!feof(stdin)) {
    if (fgets_os(buf, sizeof(buf) / sizeof(char_os), stdin) == NULL) {
      if (ferror(stdin)) {
        perror_os(T("fgets_os"));
        exit(EXIT_FAILURE);
      } else
        continue;
    }

    size_t len = strlen_os(buf);
    if (len == 0 || buf[len - 1] != T('\n')) {
      fputs_os(T("Expected a newline."), stderr);
      exit(EXIT_FAILURE);
    }
    buf[len - 1] = 0; /* erase new line */
    char_os *cursor = buf;

    /* skip note */
    if (strncmp_os(note, cursor, sizeof(note) / sizeof(char_os) - 1) != 0) {
      fputs_os(T("Unexpected input.\n"), stderr);
      exit(EXIT_FAILURE);
    }

    /* skip spaces */
    for (cursor += sizeof(note) / sizeof(char_os) - 1;
         *cursor == T(' ');
         ++cursor) ;

    if (filename_is_relative(cursor)) {
      /* skip relative path header */
      if (strncmp_os(T("./"), cursor, 2) == 0)
        cursor += 2;
    } else if (strncmp_os(cwd, cursor, cwd_len) != 0) {
      /* skip system headers */
      continue;
    } else {
      /* skip backslash */
      cursor += cwd_len + 1;
    }

    /* replace backslashes with forward slashes */
    for (char_os* p = cursor;
         (p = strchr_os(p, T('\\'))) != NULL;
         *p++ = T('/'));

    len -= cursor - buf;
    if (columns + len >= 78) {
      fputs_os(T(" \\\n"), out);
      columns = 0;
    }
    columns += len + 1;
    fprintf_os(out, T(" %s"), cursor);
  }
  fputs_os(T("\n"), out);

  free(cwd);
  rc = fclose(out);
  if (rc == EOF) {
    perror_os(T("fclose"));
    exit(EXIT_FAILURE);
  }
}

int main_os(int argc, char_os **argv)
{
  if (argc == 3 && strcmp_os(argv[1], T("encode-C-literal")) == 0) {
    encode_C_literal(argv[2]);
  } else if (argc == 5 && strcmp_os(argv[1], T("show-includes")) == 0) {
      show_includes(argv[2], argv[3], argv[4]);
  } else {
    usage();
    return 1;
  }

  return 0;
}
