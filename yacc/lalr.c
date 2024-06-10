/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Based on public-domain code from Berkeley Yacc */

#include "defs.h"

typedef
  struct shorts
    {
      struct shorts *next;
      short value;
    }
  shorts;

int tokensetsize;
short *lookaheads;
short *LAruleno;
unsigned *LA;
short *accessing_symbol;
core **state_table;
shifts **shift_table;
reductions **reduction_table;
short *goto_map;
short *from_state;
short *to_state;

short **transpose(short int **R, int n);

static int infinity;
static int maxrhs;
static int ngotos;
static unsigned *F;
static short **includes;
static shorts **lookback;
static short **R;
static short *INDEX;
static short *VERTICES;
static int top;



void set_state_table (void);
void set_accessing_symbol (void);
void set_shift_table (void);
void set_reduction_table (void);
void set_maxrhs (void);
void initialize_LA (void);
void set_goto_map (void);
void initialize_F (void);
void build_relations (void);
void compute_FOLLOWS (void);
void compute_lookaheads (void);
void digraph (short int **relation);
void add_lookback_edge (int stateno, int ruleno, int gotono);
void traverse (int i);

void lalr(void)
{
    tokensetsize = WORDSIZE(ntokens);

    set_state_table();
    set_accessing_symbol();
    set_shift_table();
    set_reduction_table();
    set_maxrhs();
    initialize_LA();
    set_goto_map();
    initialize_F();
    build_relations();
    compute_FOLLOWS();
    compute_lookaheads();
}



void set_state_table(void)
{
    state_table = xmalloc(nstates * sizeof(core *));
    for (core *sp = first_state; sp; sp = sp->next)
        state_table[sp->number] = sp;
}



void set_accessing_symbol(void)
{
    accessing_symbol = xmalloc(nstates * sizeof(short));
    for (core *sp = first_state; sp; sp = sp->next)
        accessing_symbol[sp->number] = sp->accessing_symbol;
}



void set_shift_table(void)
{
    shift_table = xmalloc(nstates * sizeof(shifts *));
    for (shifts *sp = first_shift; sp; sp = sp->next)
        shift_table[sp->number] = sp;
}



void set_reduction_table(void)
{
    reduction_table = xmalloc(nstates * sizeof(reductions *));
    for (reductions *rp = first_reduction; rp; rp = rp->next)
        reduction_table[rp->number] = rp;
}



void set_maxrhs(void)
{
  short *item_end;
  int length;
  int max;

  length = 0;
  max = 0;
  item_end = ritem + nitems;
  for (short *itemp = ritem; itemp < item_end; itemp++)
    {
      if (*itemp >= 0)
        {
          length++;
        }
      else
        {
          if (length > max) max = length;
          length = 0;
        }
    }

  maxrhs = max;
}



void initialize_LA(void)
{
  int k;
  reductions *rp;

  lookaheads = xmalloc((nstates + 1) * sizeof(short));

  k = 0;
  for (int i = 0; i < nstates; i++)
    {
      lookaheads[i] = k;
      rp = reduction_table[i];
      if (rp)
        k += rp->nreds;
    }
  lookaheads[nstates] = k;

  LA = xmalloc(k * tokensetsize * sizeof(unsigned));
  LAruleno = xmalloc(k * sizeof(short));
  lookback = xmalloc(k * sizeof(shorts *));

  k = 0;
  for (int i = 0; i < nstates; i++)
    {
      rp = reduction_table[i];
      if (rp)
        {
          for (int j = 0; j < rp->nreds; j++)
            {
              LAruleno[k] = rp->rules[j];
              k++;
            }
        }
    }
}


void set_goto_map(void)
{
  int symbol;
  int k;
  short *temp_map;
  int state2;
  int state1;

  goto_map = ((short *) xmalloc((nvars + 1) * sizeof(short))) - ntokens;
  temp_map = ((short *) xmalloc((nvars + 1) * sizeof(short))) - ntokens;

  ngotos = 0;
  for (shifts *sp = first_shift; sp; sp = sp->next)
    {
      for (int i = sp->nshifts - 1; i >= 0; i--)
        {
          symbol = accessing_symbol[sp->shift[i]];

          if (ISTOKEN(symbol)) break;

          if (ngotos == MAXSHORT)
            fatal("too many gotos");

          ngotos++;
          goto_map[symbol]++;
        }
    }

  k = 0;
  for (int i = ntokens; i < nsyms; i++)
    {
      temp_map[i] = k;
      k += goto_map[i];
    }

  for (int i = ntokens; i < nsyms; i++)
    goto_map[i] = temp_map[i];

  goto_map[nsyms] = ngotos;
  temp_map[nsyms] = ngotos;

  from_state = xmalloc(ngotos * sizeof(short));
  to_state = xmalloc(ngotos * sizeof(short));

  for (shifts *sp = first_shift; sp; sp = sp->next)
    {
      state1 = sp->number;
      for (int i = sp->nshifts - 1; i >= 0; i--)
        {
          state2 = sp->shift[i];
          symbol = accessing_symbol[state2];

          if (ISTOKEN(symbol)) break;

          k = temp_map[symbol]++;
          from_state[k] = state1;
          to_state[k] = state2;
        }
    }

  free(temp_map + ntokens);
}



/*  Map_goto maps a state/symbol pair into its numeric representation.        */

static int
map_goto(int state, int symbol)
{
    int high;
    int low;
    int middle;
    int s;

    low = goto_map[symbol];
    high = goto_map[symbol + 1];

    for (;;)
    {
        assert(low <= high);
        middle = (low + high) >> 1;
        s = from_state[middle];
        if (s == state)
            return (middle);
        else if (s < state)
            low = middle + 1;
        else
            high = middle - 1;
    }
}



void initialize_F(void)
{
  shifts *sp;
  short *edge;
  unsigned *rowp;
  short *rp;
  short **reads;
  int nedges;
  int stateno;
  int symbol;
  int nwords;

  nwords = ngotos * tokensetsize;
  F = xmalloc(nwords * sizeof(unsigned));

  reads = xmalloc(ngotos * sizeof(short *));
  edge = xmalloc((ngotos + 1) * sizeof(short));
  nedges = 0;

  rowp = F;
  for (int i = 0; i < ngotos; i++)
    {
      stateno = to_state[i];
      sp = shift_table[stateno];

      if (sp)
        {
          int j, k = sp->nshifts;

          for (j = 0; j < k; j++)
            {
              symbol = accessing_symbol[sp->shift[j]];
              if (ISVAR(symbol))
                break;
              SETBIT(rowp, symbol);
            }

          for (; j < k; j++)
            {
              symbol = accessing_symbol[sp->shift[j]];
              if (nullable[symbol])
                edge[nedges++] = map_goto(stateno, symbol);
            }

          if (nedges)
            {
              reads[i] = rp = xmalloc((nedges + 1) * sizeof(short));

              for (j = 0; j < nedges; j++)
                rp[j] = edge[j];

              rp[nedges] = -1;
              nedges = 0;
            }
        }

      rowp += tokensetsize;
    }

  SETBIT(F, 0);
  digraph(reads);

  for (int i = 0; i < ngotos; i++)
    {
      if (reads[i])
        free(reads[i]);
    }

  free(reads);
  free(edge);
}



void build_relations(void)
{
  short *rp;
  shifts *sp;
  int length;
  int nedges;
  int done;
  int state1;
  int stateno;
  int symbol1;
  int symbol2;
  short *shortp;
  short *edge;
  short *states;
  short **new_includes;

  includes = xmalloc(ngotos * sizeof(short *));
  edge = xmalloc((ngotos + 1) * sizeof(short));
  states = xmalloc((maxrhs + 1) * sizeof(short));

  for (int i = 0; i < ngotos; i++)
    {
      nedges = 0;
      state1 = from_state[i];
      symbol1 = accessing_symbol[to_state[i]];

      for (short *rulep = derives[symbol1]; *rulep >= 0; rulep++)
        {
          length = 1;
          states[0] = state1;
          stateno = state1;

          for (rp = ritem + rrhs[*rulep]; *rp >= 0; rp++)
            {
              symbol2 = *rp;
              sp = shift_table[stateno];

              for (int j = 0; j < sp->nshifts; j++)
                {
                  stateno = sp->shift[j];
                  if (accessing_symbol[stateno] == symbol2) break;
                }

              states[length++] = stateno;
            }

          add_lookback_edge(stateno, *rulep, i);

          length--;
          done = 0;
          while (!done)
            {
              done = 1;
              rp--;
              if (ISVAR(*rp))
                {
                  stateno = states[--length];
                  edge[nedges++] = map_goto(stateno, *rp);
                  if (nullable[*rp] && length > 0) done = 0;
                }
            }
        }

      if (nedges)
        {
          includes[i] = shortp = xmalloc((nedges + 1) * sizeof(short));
          for (int j = 0; j < nedges; j++)
            shortp[j] = edge[j];
          shortp[nedges] = -1;
        }
    }

  new_includes = transpose(includes, ngotos);

  for (int i = 0; i < ngotos; i++)
    if (includes[i])
      free(includes[i]);

  free(includes);

  includes = new_includes;

  free(edge);
  free(states);
}


void add_lookback_edge(int stateno, int ruleno, int gotono)
{
    int i, k;
    int found;
    shorts *sp;

    i = lookaheads[stateno];
    k = lookaheads[stateno + 1];
    found = 0;
    while (!found && i < k)
    {
        if (LAruleno[i] == ruleno)
            found = 1;
        else
            ++i;
    }
    assert(found);

    sp = xmalloc(sizeof(shorts));
    sp->next = lookback[i];
    sp->value = gotono;
    lookback[i] = sp;
}



short **
transpose(short int **R, int n)
{
  short **new_R;
  short **temp_R;
  short *nedges;
  short *sp;

  nedges = xmalloc(n * sizeof(short));

  for (int i = 0; i < n; i++)
    {
      sp = R[i];
      if (sp)
        {
          while (*sp >= 0)
            nedges[*sp++]++;
        }
    }

  new_R = xmalloc(n * sizeof(short *));
  temp_R = xmalloc(n * sizeof(short *));

  for (int i = 0; i < n; i++)
    {
      int k = nedges[i];
      if (k > 0)
        {
          sp = xmalloc((k + 1) * sizeof(short));
          new_R[i] = sp;
          temp_R[i] = sp;
          sp[k] = -1;
        }
    }

  free(nedges);

  for (int i = 0; i < n; i++)
    {
      sp = R[i];
      if (sp)
        {
          while (*sp >= 0)
            *temp_R[*sp++]++ = i;
        }
    }

  free(temp_R);

  return (new_R);
}



void compute_FOLLOWS(void)
{
  digraph(includes);
}


void compute_lookaheads(void)
{
  int n;
  unsigned *fp1, *fp2, *fp3;
  unsigned *rowp;

  rowp = LA;
  n = lookaheads[nstates];
  for (int i = 0; i < n; i++)
    {
      fp3 = rowp + tokensetsize;
      for (shorts *sp = lookback[i]; sp; sp = sp->next)
        {
          fp1 = rowp;
          fp2 = F + tokensetsize * sp->value;
          while (fp1 < fp3)
            *fp1++ |= *fp2++;
        }
      rowp = fp3;
    }

  for (int i = 0; i < n; i++)
    for (shorts *sp = lookback[i], *next; sp; sp = next)
      {
        next = sp->next;
        free(sp);
      }

  free(lookback);
  free(F);
}


void digraph(short int **relation)
{
  infinity = ngotos + 2;
  INDEX = xmalloc((ngotos + 1) * sizeof(short));
  VERTICES = xmalloc((ngotos + 1) * sizeof(short));
  top = 0;

  R = relation;

  for (int i = 0; i < ngotos; i++)
    INDEX[i] = 0;

  for (int i = 0; i < ngotos; i++)
    {
      if (INDEX[i] == 0 && R[i])
        traverse(i);
    }

  free(INDEX);
  free(VERTICES);
}



void traverse(int i)
{
  unsigned *fp1;
  unsigned *fp2;
  unsigned *fp3;
  int j;
  short *rp;

  int height;
  unsigned *base;

  VERTICES[++top] = i;
  INDEX[i] = height = top;

  base = F + i * tokensetsize;
  fp3 = base + tokensetsize;

  rp = R[i];
  if (rp)
    {
      while ((j = *rp++) >= 0)
        {
          if (INDEX[j] == 0)
            traverse(j);

          if (INDEX[i] > INDEX[j])
            INDEX[i] = INDEX[j];

          fp1 = base;
          fp2 = F + j * tokensetsize;

          while (fp1 < fp3)
            *fp1++ |= *fp2++;
        }
    }

  if (INDEX[i] == height)
    {
      for (;;)
        {
          j = VERTICES[top--];
          INDEX[j] = infinity;

          if (i == j)
            break;

          fp1 = base;
          fp2 = F + j * tokensetsize;

          while (fp1 < fp3)
            *fp2++ = *fp1++;
        }
    }
}
