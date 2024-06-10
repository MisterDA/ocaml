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

static int nvectors;
static int nentries;
static short **froms;
static short **tos;
static short *tally;
static short *width;
static short *state_count;
static short *order;
static short *base;
static short *pos;
static int maxtable;
static short *table;
static short *check;
static int lowzero;
static int high;


void free_itemsets (void);
void free_shifts (void);
void free_reductions (void);
void output_stored_text (void);
void output_transl (void);
void output_rule_data (void);
void output_yydefred (void);
void output_actions (void);
void output_debug (void);
void output_trailing_text (void);
void output_semantic_actions (void);
void output_entries (void);
void token_actions (void);
void goto_actions (void);
void sort_actions (void);
void pack_table (void);
void output_base (void);
void output_table (void);
void output_check (void);
int default_goto (int symbol);
void save_column (int symbol, int default_state);
int matching_vector (int vector);
int pack_vector (int vector);


void output(void)
{
  extern char *header[], *define_tables[];

  free_itemsets();
  free_shifts();
  free_reductions();
  write_section(header);
  output_stored_text();
  output_transl();
  output_rule_data();
  output_yydefred();
  output_actions();
  output_debug();
  free_parser();
  if (sflag){
    if (!rflag) ++outline;
    fprintf(output_file,
      "let yyact = Array.new %d (fun _ -> (failwith \"parser\" : Obj.t))\n",
      ntotalrules);
  }else{
    if (!rflag) outline += 2;
    fprintf(output_file,
      "let yyact = [|\n  (fun _ -> failwith \"parser\")\n");
  }
  output_semantic_actions();
  if (!sflag){
    if (!rflag) ++outline;
    fprintf(output_file, "|]\n");
  }
  write_section(define_tables);
  output_entries();
  output_trailing_text();
}


static void output_char(unsigned int n)
{
  n = n & 0xFF;
  putc('\\', output_file);
  putc('0' + n / 100, output_file);
  putc('0' + (n / 10) % 10, output_file);
  putc('0' + n % 10, output_file);
}

static void output_short(int n)
{
  output_char(n);
  output_char(n >> 8);
}

void output_rule_data(void)
{
    int j;


    fprintf(output_file, "let yylhs = \"");
    output_short(symbol_value[start_symbol]);

    j = 8;
    for (int i = 3; i < nrules; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        output_short(symbol_value[rlhs[i]]);
    }
    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");

    fprintf(output_file, "let yylen = \"");
    output_short(2);

    j = 8;
    for (int i = 3; i < nrules; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
          j++;

        output_short(rrhs[i + 1] - rrhs[i] - 1);
    }
    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");
}


void output_yydefred(void)
{
    int j;

    fprintf(output_file, "let yydefred = \"");
    output_short(defred[0] ? defred[0] - 2 : 0);

    j = 8;
    for (int i = 1; i < nstates; i++)
    {
        if (j < 8)
            ++j;
        else
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }

        output_short(defred[i] ? defred[i] - 2 : 0);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");
}


void output_actions(void)
{
    nvectors = 2*nstates + nvars;

    froms = xmalloc(nvectors * sizeof(short *));
    tos = xmalloc(nvectors * sizeof(short *));
    tally = xmalloc(nvectors * sizeof(short));
    width = xmalloc(nvectors * sizeof(short));

    token_actions();
    free(lookaheads);
    free(LA);
    free(LAruleno);
    free(accessing_symbol);

    goto_actions();
    free(goto_map + ntokens);
    free(from_state);
    free(to_state);

    sort_actions();
    pack_table();
    output_base();
    output_table();
    output_check();
}


void token_actions(void)
{
    int shiftcount, reducecount;
    int max, min;
    short *actionrow, *r, *s;
    action *p;

    actionrow = xmalloc(2*ntokens * sizeof(short));
    for (int i = 0; i < nstates; ++i)
    {
        if (parser[i])
        {
            for (int j = 0; j < 2*ntokens; ++j)
            actionrow[j] = 0;

            shiftcount = 0;
            reducecount = 0;
            for (p = parser[i]; p; p = p->next)
            {
                if (p->suppressed == 0)
                {
                    if (p->action_code == SHIFT)
                    {
                        ++shiftcount;
                        actionrow[p->symbol] = p->number;
                    }
                    else if (p->action_code == REDUCE && p->number != defred[i])
                    {
                        ++reducecount;
                        actionrow[p->symbol + ntokens] = p->number;
                    }
                }
            }

            tally[i] = shiftcount;
            tally[nstates+i] = reducecount;
            width[i] = 0;
            width[nstates+i] = 0;
            if (shiftcount > 0)
            {
                froms[i] = r = xmalloc(shiftcount * sizeof(short));
                tos[i] = s = xmalloc(shiftcount * sizeof(short));
                min = MAXSHORT;
                max = 0;
                for (int j = 0; j < ntokens; ++j)
                {
                    if (actionrow[j])
                    {
                        if (min > symbol_value[j])
                            min = symbol_value[j];
                        if (max < symbol_value[j])
                            max = symbol_value[j];
                        *r++ = symbol_value[j];
                        *s++ = actionrow[j];
                    }
                }
                width[i] = max - min + 1;
            }
            if (reducecount > 0)
            {
                froms[nstates+i] = r = xmalloc(reducecount * sizeof(short));
                tos[nstates+i] = s = xmalloc(reducecount * sizeof(short));
                min = MAXSHORT;
                max = 0;
                for (int j = 0; j < ntokens; ++j)
                {
                    if (actionrow[ntokens+j])
                    {
                        if (min > symbol_value[j])
                            min = symbol_value[j];
                        if (max < symbol_value[j])
                            max = symbol_value[j];
                        *r++ = symbol_value[j];
                        *s++ = actionrow[ntokens+j] - 2;
                    }
                }
                width[nstates+i] = max - min + 1;
            }
        }
    }
    free(actionrow);
}

void goto_actions(void)
{
    int j, k;

    state_count = xmalloc(nstates * sizeof(short));

    k = default_goto(start_symbol + 1);
    fprintf(output_file, "let yydgoto = \"");
    output_short(k);

    save_column(start_symbol + 1, k);

    j = 8;
    for (int i = start_symbol + 2; i < nsyms; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        k = default_goto(i);
        output_short(k);
        save_column(i, k);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");
    free(state_count);
}

int
default_goto(int symbol)
{
    int m;
    int n;
    int default_state;
    int max;

    m = goto_map[symbol];
    n = goto_map[symbol + 1];

    if (m == n) return (0);

    for (int i = 0; i < nstates; i++)
        state_count[i] = 0;

    for (int i = m; i < n; i++)
        state_count[to_state[i]]++;

    max = 0;
    default_state = 0;
    for (int i = 0; i < nstates; i++)
    {
        if (state_count[i] > max)
        {
            max = state_count[i];
            default_state = i;
        }
    }

    return (default_state);
}



void save_column(int symbol, int default_state)
{
    int m;
    int n;
    short *sp;
    short *sp1;
    short *sp2;
    int count;
    int symno;

    m = goto_map[symbol];
    n = goto_map[symbol + 1];

    count = 0;
    for (int i = m; i < n; i++)
    {
        if (to_state[i] != default_state)
            ++count;
    }
    if (count == 0) return;

    symno = symbol_value[symbol] + 2*nstates;

    froms[symno] = sp1 = sp = xmalloc(count * sizeof(short));
    tos[symno] = sp2 = xmalloc(count * sizeof(short));

    for (int i = m; i < n; i++)
    {
        if (to_state[i] != default_state)
        {
            *sp1++ = from_state[i];
            *sp2++ = to_state[i];
        }
    }

    tally[symno] = count;
    width[symno] = sp1[-1] - sp[0] + 1;
}

void sort_actions(void)
{
  int j;
  int k;
  int t;
  int w;

  order = xmalloc(nvectors * sizeof(short));
  nentries = 0;

  for (int i = 0; i < nvectors; i++)
    {
      if (tally[i] > 0)
        {
          t = tally[i];
          w = width[i];
          j = nentries - 1;

          while (j >= 0 && (width[order[j]] < w))
            j--;

          while (j >= 0 && (width[order[j]] == w) && (tally[order[j]] < t))
            j--;

          for (k = nentries - 1; k > j; k--)
            order[k + 1] = order[k];

          order[j + 1] = i;
          nentries++;
        }
    }
}


void pack_table(void)
{
    int place;
    int state;

    base = xmalloc(nvectors * sizeof(short));
    pos = xmalloc(nentries * sizeof(short));

    maxtable = 1000;
    table = xmalloc(maxtable * sizeof(short));
    check = xmalloc(maxtable * sizeof(short));

    lowzero = 0;
    high = 0;

    for (int i = 0; i < maxtable; i++)
        check[i] = -1;

    for (int i = 0; i < nentries; i++)
    {
        state = matching_vector(i);

        if (state < 0)
            place = pack_vector(i);
        else
            place = base[state];

        pos[i] = place;
        base[order[i]] = place;
    }

    for (int i = 0; i < nvectors; i++)
    {
        if (froms[i])
            free(froms[i]);
        if (tos[i])
            free(tos[i]);
    }

    free(froms);
    free(tos);
    free(pos);
}


/*  The function matching_vector determines if the vector specified by    */
/*  the input parameter matches a previously considered vector.  The      */
/*  test at the start of the function checks if the vector represents     */
/*  a row of shifts over terminal symbols or a row of reductions, or a    */
/*  column of shifts over a nonterminal symbol.  Berkeley Yacc does not   */
/*  check if a column of shifts over a nonterminal symbols matches a      */
/*  previously considered vector.  Because of the nature of LR parsing    */
/*  tables, no two columns can match.  Therefore, the only possible       */
/*  match would be between a row and a column.  Such matches are          */
/*  unlikely.  Therefore, to save time, no attempt is made to see if a    */
/*  column matches a previously considered vector.                        */
/*                                                                        */
/*  Matching_vector is poorly designed.  The test could easily be made    */
/*  faster.  Also, it depends on the vectors being in a specific          */
/*  order.                                                                */

int
matching_vector(int vector)
{
    int i;
    int t;
    int w;
    int match;

    i = order[vector];
    if (i >= 2*nstates)
        return (-1);

    t = tally[i];
    w = width[i];

    for (int prev = vector - 1; prev >= 0; prev--)
    {
        int j = order[prev];
        if (width[j] != w || tally[j] != t)
            return (-1);

        match = 1;
        for (int k = 0; match && k < t; k++)
        {
            if (tos[j][k] != tos[i][k] || froms[j][k] != froms[i][k])
                match = 0;
        }

        if (match)
            return (j);
    }

    return (-1);
}



int
pack_vector(int vector)
{
    int i, j;
    int t;
    int loc;
    int ok;
    short *from;
    short *to;
    int newmax;

    i = order[vector];
    t = tally[i];
    assert(t);

    from = froms[i];
    to = tos[i];

    j = lowzero - from[0];
    for (int k = 1; k < t; ++k)
        if (lowzero - from[k] > j)
            j = lowzero - from[k];
    for (;; ++j)
    {
        if (j == 0)
            continue;
        ok = 1;
        for (int k = 0; ok && k < t; k++)
        {
            loc = j + from[k];
            if (loc >= maxtable)
            {
                if (loc >= MAXTABLE)
                    fatal("maximum table size exceeded");

                newmax = maxtable;
                do { newmax += 200; } while (newmax <= loc);
                table = realloc(table, newmax*sizeof(short));
                if (table == 0) no_space();
                check = realloc(check, newmax*sizeof(short));
                if (check == 0) no_space();
                for (int l = maxtable; l < newmax; ++l)
                {
                    table[l] = 0;
                    check[l] = -1;
                }
                maxtable = newmax;
            }

            if (check[loc] != -1)
                ok = 0;
        }
        for (int k = 0; ok && k < vector; k++)
        {
            if (pos[k] == j)
                ok = 0;
        }
        if (ok)
        {
            for (int k = 0; k < t; k++)
            {
                loc = j + from[k];
                table[loc] = to[k];
                check[loc] = from[k];
                if (loc > high) high = loc;
            }

            while (lowzero < maxtable && check[lowzero] != -1)
                ++lowzero;

            return (j);
        }
    }
}



void output_base(void)
{
    int j;

    fprintf(output_file, "let yysindex = \"");
    output_short(base[0]);

    j = 8;
    for (int i = 1; i < nstates; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        output_short(base[i]);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");

    fprintf(output_file, "let yyrindex = \"");
    output_short(base[nstates]);

    j = 8;
    for (int i = nstates + 1; i < 2 * nstates; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        output_short(base[i]);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");

    fprintf(output_file, "let yygindex = \"");
    output_short(base[2*nstates]);

    j = 8;
    for (int i = 2 * nstates + 1; i < nvectors - 1; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        output_short(base[i]);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");
    free(base);
}



void output_table(void)
{
    int j;

    ++outline;
    fprintf(code_file, "let yytablesize = %d\n", high);
    fprintf(output_file, "let yytable = \"");
    output_short(table[0]);

    j = 8;
    for (int i = 1; i <= high; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        output_short(table[i]);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");
    free(table);
}



void output_check(void)
{
    int j;

    fprintf(output_file, "let yycheck = \"");
    output_short(check[0]);

    j = 8;
    for (int i = 1; i <= high; i++)
    {
        if (j >= 8)
        {
            if (!rflag) ++outline;
            fprintf(output_file, "\\\n");
            j = 1;
        }
        else
            ++j;

        output_short(check[i]);
    }

    if (!rflag) outline += 2;
    fprintf(output_file, "\"\n\n");
    free(check);
}


void output_transl(void)
{
  ++outline;
  fprintf(code_file, "let yytransl_const = [|\n");
  for (int i = 0; i < ntokens; i++) {
    if (symbol_true_token[i] && symbol_tag[i] == NULL) {
      ++outline;
      fprintf(code_file, "  %3d (* %s *);\n", symbol_value[i], symbol_name[i]);
    }
  }
  outline += 2;
  fprintf(code_file, "    0|]\n\n");
  ++outline;
  fprintf(code_file, "let yytransl_block = [|\n");
  for (int i = 0; i < ntokens; i++) {
    if (symbol_true_token[i] && symbol_tag[i] != NULL) {
      ++outline;
      fprintf(code_file, "  %3d (* %s *);\n", symbol_value[i], symbol_name[i]);
    }
  }
  outline += 2;
  fprintf(code_file, "    0|]\n\n");
}

void output_stored_text(void)
{
    int c;
    FILE *in, *out;

    fclose(text_file);
    text_file = fopen_os(text_file_name, T("r"));
    if (text_file == NULL)
        open_error(text_file_name);
    in = text_file;
    if ((c = getc(in)) == EOF)
        return;
    out = code_file;
    if (c ==  '\n')
        ++outline;
    putc(c, out);
    while ((c = getc(in)) != EOF)
    {
        if (c == '\n')
            ++outline;
        putc(c, out);
    }
    if (!lflag)
        fprintf(out, line_format, ++outline + 1, code_file_name_disp);
}


void output_debug(void)
{
  ++outline;
  fprintf(code_file, "let yynames_const = \"\\\n");
  for (int i = 0; i < ntokens; i++) {
    if (symbol_true_token[i] && symbol_tag[i] == NULL) {
      ++outline;
      fprintf(code_file, "  %s\\000\\\n", symbol_name[i]);
    }
  }
  outline += 2;
  fprintf(code_file, "  \"\n\n");
  ++outline;
  fprintf(code_file, "let yynames_block = \"\\\n");
  for (int i = 0; i < ntokens; i++) {
    if (symbol_true_token[i] && symbol_tag[i] != NULL) {
      ++outline;
      fprintf(code_file, "  %s\\000\\\n", symbol_name[i]);
    }
  }
  outline += 2;
  fprintf(code_file, "  \"\n\n");
}

void output_trailing_text(void)
{
    int c, last;
    FILE *in, *out;

    if (line == 0)
        return;

    in = input_file;
    out = code_file;

    ++outline;
    fprintf (out, ";;\n");

    c = *cptr;
    if (c == '\n')
    {
        ++lineno;
        if ((c = getc(in)) == EOF)
            return;
        if (!lflag)
        {
            ++outline;
            fprintf(out, line_format, lineno, input_file_name_disp);
        }
        if (c == '\n')
            ++outline;
        putc(c, out);
        last = c;
    }
    else
    {
        if (!lflag)
        {
            ++outline;
            fprintf(out, line_format, lineno, input_file_name_disp);
        }
        do { putc(c, out); } while ((c = *++cptr) != '\n');
        ++outline;
        putc('\n', out);
        last = '\n';
    }


    while ((c = getc(in)) != EOF)
    {
        if (c == '\n')
            ++outline;
        putc(c, out);
        last = c;
    }

    if (last != '\n')
    {
        ++outline;
        putc('\n', out);
    }
    if (!lflag)
        fprintf(out, line_format, ++outline + 1, code_file_name_disp);
}


void copy_file(FILE **file, char_os *file_name)
{
  int c, last;
  FILE *out = code_file;
  int state = 0;

  fclose(*file);
    *file = fopen_os(file_name, T("r"));
    if (*file == NULL)
        open_error(file_name);

    last = '\n';

    while ((c = getc(*file)) != EOF)
    {
      switch (c){
      case '\n': state = 1; break;
      case '#': state = (state == 1) ? 2 : 0; break;
      case ' ': state = (state == 2) ? 3 : 0; break;
      case '0':
        if (state == 3){
          fprintf (out, "%d \"%s", outline+2, code_file_name_disp);
          c = '"';
        }
        state = 0;
        break;
      default: state = 0; break;
      }
      if (c == '\n') ++outline;
      putc(c, out);
      last = c;
    }

    if (last != '\n')
    {
        ++outline;
        putc('\n', out);
    }

}

void output_semantic_actions(void)
{
  copy_file (&action_file, action_file_name);
}

void output_entries(void)
{
  copy_file (&entry_file, entry_file_name);
}

void free_itemsets(void)
{
    free(state_table);
    for (core *cp = first_state, *next; cp; cp = next)
    {
        next = cp->next;
        free(cp);
    }
}


void free_shifts(void)
{
    free(shift_table);
    for (shifts *sp = first_shift, *next; sp; sp = next)
    {
        next = sp->next;
        free(sp);
    }
}



void free_reductions(void)
{
    free(reduction_table);
    for (reductions *rp = first_reduction, *next; rp; rp = next)
    {
        next = rp->next;
        free(rp);
    }
}
