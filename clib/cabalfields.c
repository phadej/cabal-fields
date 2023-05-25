#include "cabalfields.h"

/*
#define NDEBUG 1
*/

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/********************************************************************
 * Assertions
 ********************************************************************/

static void cf_panic(const char *str) {
    fprintf(stderr, "%s\n", str);
    exit(1);
}

#define ASSERT_STATE(s, st) \
    assert(s->state == st); \
    assert(s->pos <= s->size); \
    assert(s->nam <= s->pos); \
    assert(s->ind <= s->nam); \
    assert(s->chk <= s->ind); \
    assert(s->stack_size <= CF_MAX_STACK_SIZE);

/********************************************************************
 * Parser state enumeration forward declarations.
 ********************************************************************/

cf_token_type cf_parse_b0(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_bi(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_bc(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_bq(cf_parser_state *s, cf_token *t);

cf_token_type cf_parse_an(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_xn(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_ac(cf_parser_state *s, cf_token *t);

cf_token_type cf_parse_s0(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_sa(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_sl(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_sg(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_sc(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_sq(cf_parser_state *s, cf_token *t);

cf_token_type cf_parse_f0(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_fn(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_fl(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_fg(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_fc(cf_parser_state *s, cf_token *t);
cf_token_type cf_parse_fq(cf_parser_state *s, cf_token *t);

cf_token_type cf_pop_eof(cf_parser_state *s, cf_token *t);
cf_token_type cf_pop_name(cf_parser_state *s, cf_token *t);

cf_token_type cf_eof(cf_parser_state *s, cf_token *t);

/********************************************************************
 * Printing & debugging helpers
 ********************************************************************/

char *cf_splice(cf_parser_state *s, size_t bgn, size_t to) {
    size_t size = to - bgn;
    char *str = malloc(size + 1);
    memcpy(str, s->contents + bgn, size);
    str[size] = '\0';
    return str;
}

const char *cf_parser_str(cf_parser p) {
    if (p == cf_parse_b0) return "B0";
    if (p == cf_parse_bc) return "BC";
    if (p == cf_parse_bq) return "BQ";
    if (p == cf_parse_bi) return "BI";

    if (p == cf_parse_an) return "AN";
    if (p == cf_parse_xn) return "XN";
    if (p == cf_parse_ac) return "AC";

    if (p == cf_parse_s0) return "S0";
    if (p == cf_parse_sa) return "SA";
    if (p == cf_parse_sg) return "SG";
    if (p == cf_parse_sl) return "SL";
    if (p == cf_parse_sc) return "SC";
    if (p == cf_parse_sq) return "SQ";

    if (p == cf_parse_f0) return "F0";
    if (p == cf_parse_fn) return "FN";
    if (p == cf_parse_fl) return "FL";
    if (p == cf_parse_fg) return "FG";
    if (p == cf_parse_fc) return "FC";
    if (p == cf_parse_fq) return "FQ";

    if (p == cf_pop_eof)  return "PE";
    if (p == cf_pop_name) return "PI";
    if (p == cf_eof)      return "EO";

    return "??";
}

/* TODO: use single printf */
int cf_print_parser_state(FILE *fp, cf_parser_state *s) {
    fprintf(fp, "input: %p (%ld); ", s->contents, s->size);
    fprintf(fp, "loc: %03d:%03d; ", s->loc.row, s->loc.col);
    fprintf(fp, "chk: %4ld; ", s->chk);
    fprintf(fp, "ind: %4ld (%2d); ", s->ind, (int) (s->ind - s->chk));
    fprintf(fp, "nam: %4ld (%2d); ", s->nam, (int) (s->nam - s->ind));
    fprintf(fp, "pos: %4ld (%2d); ", s->pos, (int) (s->pos - s->nam));
    fprintf(fp, "state: %s; ", cf_parser_str(s->state));
    fprintf(fp, "stack:");
    for (size_t i = 0; i < s->stack_size; i++) {
        fprintf(fp, " %d", s->stack[i]);
    }
    return fprintf(fp, "\n");
}

const char *cf_token_type_str(cf_token_type t) {
    switch (t) {
#ifdef CF_HAS_TK_SKIP
        case CF_TK_SKIP:                     return "skip";
#endif
        case CF_TK_UNEXPECTED_CHAR:          return "unexpected_char";
        case CF_TK_INCONSISTENT_INDENTATION: return "inconsistent_indentation";
		case CF_TK_STACKOVERFLOW:            return "stack_overflow";
        case CF_TK_EOF:                      return "eof";
        case CF_TK_COMMENT:                  return "comment";
        case CF_TK_FIELD:                    return "field";
        case CF_TK_FIELDLINE:                return "fieldline";
        case CF_TK_SECTION:                  return "section";
        case CF_TK_FIELD_END:                return "field_end";
        case CF_TK_SECTION_END:              return "section_end";
        default:                             return "unknown";
    }
}

int cf_print_token(FILE *fp, cf_token_type e, cf_token *t) {
    switch (e) {
#ifdef CF_HAS_TK_SKIP
        case CF_TK_SKIP: return fprintf(fp, "skip\n");
#endif
        case CF_TK_UNEXPECTED_CHAR:
            return fprintf(fp, "error at %d:%d state %s, unexpected '%c'\n", t->error_uc.loc.row, t->error_uc.loc.col, t->error_uc.state, (char) t->error_uc.c);

        case CF_TK_INCONSISTENT_INDENTATION:
            return fprintf(fp, "error at %d:%d, inconsistent indentation\n", t->error_ii.loc.row, t->error_ii.loc.col);

        case CF_TK_STACKOVERFLOW:
            return fprintf(fp, "error at %d:%d, stack overflow\n", t->error_so.loc.row, t->error_so.loc.col);

        case CF_TK_EOF:
			return fprintf(fp, "eof\n");

        case CF_TK_COMMENT:
			return fprintf(fp, "comment @%03d:%03d %ld..%ld\n", t->comment.loc.row, t->comment.loc.col, t->comment.bgn, t->comment.end);

        case CF_TK_FIELD:
            return fprintf(fp, "field at %d:%d %ld..%ld\n", t->field.name_loc.row, t->field.name_loc.col, t->field.name_bgn, t->field.name_end);

        case CF_TK_FIELDLINE:
            return fprintf(fp, "fieldline at %d:%d %ld..%ld\n", t->fieldline.line_loc.row, t->fieldline.line_loc.col, t->fieldline.line_bgn, t->fieldline.line_end);

        case CF_TK_SECTION:
            return fprintf(fp, "section at %d:%d %ld..%ld %ld..%ld\n", t->section.name_loc.row, t->section.name_loc.col, t->section.name_bgn, t->section.name_end, t->section.args_bgn, t->section.args_end);

        default: return fprintf(fp, "unknown: %d\n", e);
    }
}

int cf_print_token_ex(FILE *fp, cf_parser_state *s, cf_token_type e, cf_token *t) {
    char *str1 = NULL;
    char *str2 = NULL;
    int r = 0;
    switch (e) {
#ifdef CF_HAS_TK_SKIP
        case CF_TK_SKIP:
            return fprintf(fp, "skip\n");
#endif

        case CF_TK_UNEXPECTED_CHAR:
            return fprintf(fp, "error at %d:%d state %s, unexpected '%c'\n", t->error_uc.loc.row, t->error_uc.loc.col, t->error_uc.state, (char) t->error_uc.c);

        case CF_TK_INCONSISTENT_INDENTATION:
            return fprintf(fp, "error at %d:%d, inconsistent indentation\n", t->error_ii.loc.row, t->error_ii.loc.col);

        case CF_TK_STACKOVERFLOW:
            return fprintf(fp, "error at %d:%d, stack overflow\n", t->error_so.loc.row, t->error_so.loc.col);

        case CF_TK_EOF:
            return fprintf(fp, "eof at %d:%d, offset %ld\n", t->eof.loc.row, t->eof.loc.col, t->eof.pos);

        case CF_TK_COMMENT:
            str1 = cf_splice(s, t->comment.bgn, t->comment.end);
            r = fprintf(fp, "comment at %d:%d \"%s\"\n", t->comment.loc.row, t->comment.loc.col, str1);
            free(str1);
            return r;

        case CF_TK_FIELD:
            str1 = cf_splice(s, t->field.name_bgn, t->field.name_end);
            r = fprintf(fp, "field at %d:%d \"%s\"\n", t->field.name_loc.row, t->field.name_loc.col, str1);
            free(str1);
            return r;

        case CF_TK_FIELDLINE:
            str1 = cf_splice(s, t->fieldline.line_bgn, t->fieldline.line_end);
            r = fprintf(fp, "fieldline at %d:%d \"%s\"\n", t->fieldline.line_loc.row, t->fieldline.line_loc.col, str1);
            free(str1);
            return r;

        case CF_TK_SECTION:
            str1 = cf_splice(s, t->section.name_bgn, t->section.name_end);
            str2 = cf_splice(s, t->section.args_bgn, t->section.args_end);
            r = fprintf(fp, "section at %d:%d \"%s\" \"%s\"\n", t->section.name_loc.row, t->section.name_loc.col, str1, str2);
            free(str1);
            free(str2);
            return r;

        case CF_TK_FIELD_END:
            return fprintf(fp, "field end\n");

        case CF_TK_SECTION_END:
            return fprintf(fp, "section end\n");

        default: return fprintf(fp, "unknown: %d\n", e);
    }
}

/********************************************************************
 * Parser (state)
 ********************************************************************/

cf_token_type cf_next(cf_parser_state *s, cf_token *t) {
    return s->state(s, t);
}

void cf_reset(cf_parser_state *s, const uint8_t *contents, size_t size) {
    s->contents = contents;
    s->size = size;
    s->loc.row = 1;
    s->loc.col = 1;
    s->chk = 0;
    s->ind = 0;
    s->nam = 0;
    s->pos = 0;
    s->state = cf_parse_b0;
    s->stack_size = 0;
}

/********************************************************************
 * Helpers
 ********************************************************************/

#define CF_ADVANCE_LOC(s, a, b) cf_advance_loc(s->contents, a, b, &(s->loc))

void cf_advance_loc(const uint8_t *contents, size_t a, size_t b, cf_srcloc *loc) {
start:
    if (a >= b) return;

    switch (contents[a]) {
        case '\n':
            a += 1;
            loc->row += 1;
            loc->col = 1;
            goto start;

        case '\r':
            a += 1;
            loc->row += 1;
            loc->col = 1;
            goto cr;

        default:
            a += 1;
            loc->col += 1;
            goto start;

    }

cr:
    if (a >= b) return;

    switch (contents[a]) {
        case '\n':
            a += 1;
            goto start;

        case '\r':
            a += 1;
            loc->row += 1;
            loc->col = 1;
            goto cr;

        default:
            a += 1;
            loc->col += 1;
            goto start;
    }
}

#define CF_ISCOMMENT(s) (s->pos + 1 < s->size && s->contents[s->pos] == '-' && s->contents[s->pos+1] == '-')

/********************************************************************
 * Character set functions
 ********************************************************************/

static bool cf_iscomment(uint8_t c) {
    return c == '\t' || (0x20 <= c && c <0x7f) || c >= 0x80;
}

/* field content and comment characters are the same. */
#define cf_isfieldlayout cf_iscomment

static bool cf_isname(uint8_t c) {
    return c == '\'' || c == '-' || c == '.' || ('0' <= c && c <= '9') || c == ';' || ('A' <= c && c <= 'Z') || ('_' <= c && c < '{') || c >= 0x80;
}

static bool cf_isspace(uint8_t c) {
    return c == ' ' || c == '\t';
}

static bool cf_issectionarg_head(uint8_t c) {
    return (' ' <= c && c < 0x7f && c != ':' && c != '{' && c != '}') || c >= 0x80;
}

static bool cf_issectionarg_tail(uint8_t c) {
    return (' ' <= c && c < 0x7f && c != '{' && c != '}') || c >= 0x80;
}

/********************************************************************
 * Parser state functions
 ********************************************************************/

#define CF_UNEXPECTED(s, t) \
    t->error_uc.loc = s->loc; \
    t->error_uc.state = cf_parser_str(s->state); \
    t->error_uc.c = s->contents[s->pos]; \
    return CF_TK_UNEXPECTED_CHAR; \

#define CF_MUNCH(s,curr,predicate) while (curr < s->size && predicate(s->contents[curr])) { curr += 1; }

#ifdef CF_HAS_TK_SKIP
#define CF_SKIP(s, t) CF_TK_SKIP
#else
#define CF_SKIP(s, t) cf_next(s, t)
#endif

/********************************************************************
 * Parser state functions: popping from stack
 ********************************************************************/

cf_token_type cf_pop_eof(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_pop_eof);
    assert(s->chk == s->pos);
    assert(s->stack_size > 0);

    if (s->stack_size > 1) {
        s->stack_size--;
        return CF_TK_SECTION_END;
    } else { /* s->stack_size == 1 */
        s->stack_size--;

        s->state = cf_eof;
        return CF_SKIP(s, t);
    }
}

cf_token_type cf_pop_name(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_pop_name);

    if (s->stack_size > 0) {
        int j = s->ind - s->chk; /* indent */
        int i = s->stack[s->stack_size - 1];

        if (i == j) {
            /*
            size_t curr = s->pos;
            CF_MUNCH(s, curr, cf_isspace);

            s->pos = curr;
            */

            s->state = cf_parse_an;

            return CF_SKIP(s, t);

        } else if (i > j) {
            s->stack_size -= 1;
            return CF_TK_SECTION_END;

        } else {
            /* inconsistent indentation */
            t->error_ii.loc = s->loc;
            return CF_TK_INCONSISTENT_INDENTATION;
        }
    } else {
        s-> state = cf_eof;
        return CF_SKIP(s, t);
    }
}

cf_token_type cf_eof(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_eof);
    assert(s->chk == s->pos);
    assert(s->stack_size == 0);

    t->eof.loc = s->loc;
    t->eof.pos = s->pos;
    return CF_TK_EOF;
}

/********************************************************************
 * Parser state functions: beginning
 ********************************************************************/

/* The beginning of the file. */
cf_token_type cf_parse_b0(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_b0);
    assert(s->chk == s->pos);
    assert(s->loc.col == 1);
    assert(s->stack_size == 0);

    if (s->pos >= s->size) {
        s->state = cf_eof;
        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;
        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_bc;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->stack[s->stack_size++] = 0;
        s->state = cf_parse_an;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == ' ' || s->contents[s->pos] == '\t') {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isspace);
        s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_bi;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/* After the beginning indent */
cf_token_type cf_parse_bi(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_bi);
    assert(s->ind == s->pos);
    assert(s->stack_size == 0);

    if (s->pos >= s->size) {
        s->chk = s->pos;
        s->state = cf_eof;
        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_b0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_b0;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;
        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_bc;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->stack[s->stack_size++] = s->ind - s->chk;
        s->state = cf_parse_an;

        return CF_SKIP(s, t);

    }  else {
        CF_UNEXPECTED(s, t);
    }

    cf_panic("TODO BI");
    return 0;
}

/* While at the beginning after comment start @--@ token. */
cf_token_type cf_parse_bc(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_bc);
    assert(s->chk == s->pos);
    assert(s->stack_size == 0);

    size_t curr = s->pos;
    CF_MUNCH(s, curr, cf_iscomment);

    t->comment.loc = s->loc;
    t->comment.bgn = s->pos;
    t->comment.end = curr;

    CF_ADVANCE_LOC(s, s->chk, curr);
    s->chk = s->ind = s->nam = s->pos = curr;
    s->state = cf_parse_bq;

    return CF_TK_COMMENT;
}

/* While at the beginning after comment contents. */
cf_token_type cf_parse_bq(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_bq);
    assert(s->chk == s->pos);
    assert(s->stack_size == 0);

    if (s->pos >= s->size) {
        s->state = cf_eof;
        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_b0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_b0;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/********************************************************************
 * Parser state functions: after name and after colon
 ********************************************************************/

/* After a name (and whitespace) */
cf_token_type cf_parse_an(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_an);
    assert(s->stack_size > 0);

    if (s->pos >= s->size) {
        CF_ADVANCE_LOC(s, s->chk, s->ind);
        t->section.name_loc = s->loc;
        t->section.name_bgn = s->ind;
        t->section.name_end = s->nam;

        CF_ADVANCE_LOC(s, s->ind, s->pos);
        t->section.args_loc = s->loc;
        t->section.args_bgn = s->pos;
        t->section.args_end = s->pos;

        s->chk = s->ind = s->nam = s->pos;
        s->state = cf_parse_xn;

        return CF_TK_SECTION;

    } else if (s->contents[s->pos] == ':') {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isspace);

        CF_ADVANCE_LOC(s, s->chk, s->ind);
        t->field.name_loc = s->loc;
        t->field.name_bgn = s->ind;
        t->field.name_end = s->nam;

        CF_ADVANCE_LOC(s, s->ind, s->pos);
        t->field.colon_loc = s->loc;

        CF_ADVANCE_LOC(s, s->pos, curr);

        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_ac;

        return CF_TK_FIELD;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, s->ind);
        t->section.name_loc = s->loc;
        t->section.name_bgn = s->ind;
        t->section.name_end = s->nam;

        CF_ADVANCE_LOC(s, s->ind, s->pos);
        t->section.args_loc = s->loc;
        t->section.args_bgn = s->pos;
        t->section.args_end = s->pos;

        CF_ADVANCE_LOC(s, s->pos, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_TK_SECTION;

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, s->ind);
        t->section.name_loc = s->loc;
        t->section.name_bgn = s->ind;
        t->section.name_end = s->nam;

        CF_ADVANCE_LOC(s, s->ind, s->pos);
        t->section.args_loc = s->loc;
        t->section.args_bgn = s->pos;
        t->section.args_end = s->pos;

        CF_ADVANCE_LOC(s, s->pos, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_TK_SECTION;

    } else if (cf_issectionarg_head(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_issectionarg_tail);

        CF_ADVANCE_LOC(s, s->chk, s->ind);
        t->section.name_loc = s->loc;
        t->section.name_bgn = s->ind;
        t->section.name_end = s->nam;

        CF_ADVANCE_LOC(s, s->ind, s->pos);
        t->section.args_loc = s->loc;
        t->section.args_bgn = s->pos;
        t->section.args_end = curr;

        CF_ADVANCE_LOC(s, s->pos, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_sa;

        return CF_TK_SECTION;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/* After a name (and whitespace) and eof */
cf_token_type cf_parse_xn(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_xn);
    assert(s->chk == s->pos);
    assert(s->stack_size > 0);
    assert(s->pos >= s->size);

    s->state = cf_pop_eof;

    return CF_TK_SECTION_END;
}

/* After colon (and whitespace). */
cf_token_type cf_parse_ac(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_ac);
    assert(s->chk == s->pos);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_FIELD_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (cf_isfieldlayout(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isfieldlayout);

        t->fieldline.line_loc = s->loc;
        t->fieldline.line_bgn = s->pos;
        t->fieldline.line_end = curr;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_fn;

        return CF_TK_FIELDLINE;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/********************************************************************
 * Parser state functions: sections
 ********************************************************************/

/* Immediately after section header. After a newline (at initial column).
*/
cf_token_type cf_parse_s0(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_s0);
    assert(s->chk == s->pos);
    assert(s->loc.col == 1);
    assert(s->stack_size > 0);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_SECTION_END;

    } else if (s->contents[s->pos] == ' ' || s->contents[s->pos] == '\t') {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isspace);
        s->ind = s->nam = s->pos = curr;

        int i = s->stack[s->stack_size - 1];
        int j = s->ind - s->chk;

        if (j > i) {
            s->state = cf_parse_sg;
            return CF_SKIP(s, t);

        } else {
            s->state = cf_parse_sl;
            return CF_SKIP(s, t);
        }
    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_sc;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->state = cf_pop_name;

        return CF_TK_SECTION_END;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/* After section arguments. Expecting newline. */
cf_token_type cf_parse_sa(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_sa);
    assert(s->chk == s->pos);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_SECTION_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/* While parsing section contents after indent larger than current. */
cf_token_type cf_parse_sg(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_sg);
    assert(s->ind == s->pos);

    if (s->pos >= s->size) {
        s->chk = s->pos;
        s->state = cf_pop_eof;
        return CF_TK_SECTION_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_sc;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        if (s->stack_size == CF_MAX_STACK_SIZE) {
            t->error_so.loc = s->loc;
            return CF_TK_STACKOVERFLOW;
        }

        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->stack[s->stack_size++] = s->ind - s->chk;
        s->state = cf_parse_an;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}

cf_token_type cf_parse_sl(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_sl);
    assert(s->ind == s->pos);

    if (s->pos >= s->size) {
        s->chk = s->pos;
        s->state = cf_pop_eof;
        return CF_TK_SECTION_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_sc;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->state = cf_pop_name;

        return CF_TK_SECTION_END;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

cf_token_type cf_parse_sc(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_sc);
    assert(s->chk == s->pos);

    size_t curr = s->pos;
    CF_MUNCH(s, curr, cf_iscomment);

    t->comment.loc = s->loc;
    t->comment.bgn = s->pos;
    t->comment.end = curr;

    CF_ADVANCE_LOC(s, s->chk, curr);
    s->chk = s->ind = s->nam = s->pos = curr;
    s->state = cf_parse_sq;

    return CF_TK_COMMENT;
}

cf_token_type cf_parse_sq(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_sq);
    assert(s->chk == s->pos);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_SECTION_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_s0;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/********************************************************************
 * Parser state functions: fields
 ********************************************************************/

cf_token_type cf_parse_fn(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_fn);
    assert(s->chk == s->pos);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_FIELD_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}

/* After colon and newline or fieldline and newline. After a newline (at initial column). */
cf_token_type cf_parse_f0(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_f0);
    assert(s->chk == s->pos);
    assert(s->loc.col == 1);
    assert(s->stack_size > 0);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_FIELD_END;

    } else if (s->contents[s->pos] == ' ' || s->contents[s->pos] == '\t') {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isspace);

        s->ind = s->nam = s->pos = curr;

        int i = s->stack[s->stack_size - 1];
        int j = s->ind - s->chk;

        if (j > i) {
            s->state = cf_parse_fg;
            return CF_SKIP(s, t);

        } else {
            s->state = cf_parse_fl;
            return CF_SKIP(s, t);
        }

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_fc;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->state = cf_pop_name;

        return CF_TK_FIELD_END;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

cf_token_type cf_parse_fl(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_fl);
    assert(s->ind == s->pos);

    if (s->pos >= s->size) {
        s->chk = s->pos;
        s->state = cf_pop_eof;
        return CF_TK_FIELD_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_fc;

        return CF_SKIP(s, t);

    } else if (cf_isname(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isname);
        size_t skip = curr;
        CF_MUNCH(s, skip, cf_isspace);

        s->nam = curr;
        s->pos = skip;
        s->state = cf_pop_name;

        return CF_TK_FIELD_END;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

cf_token_type cf_parse_fg(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_fg);
    assert(s->ind == s->pos);

    if (s->pos >= s->size) {
        s->chk = s->pos;
        s->state = cf_pop_eof;
        return CF_TK_FIELD_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (CF_ISCOMMENT(s)) {
        size_t curr = s->pos + 2;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_fc;

        return CF_SKIP(s, t);

    } else if (cf_isfieldlayout(s->contents[s->pos])) {
        size_t curr = s->pos + 1;
        CF_MUNCH(s, curr, cf_isfieldlayout);

        CF_ADVANCE_LOC(s, s->chk, s->pos);
        t->fieldline.line_loc = s->loc;
        t->fieldline.line_bgn = s->pos;
        t->fieldline.line_end = curr;

        CF_ADVANCE_LOC(s, s->pos, curr);
        s->chk = s->ind = s->nam = s->pos = curr;
        s->state = cf_parse_fn;

        return CF_TK_FIELDLINE;

    } else {
        CF_UNEXPECTED(s, t);
    }
}

cf_token_type cf_parse_fc(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_fc);
    assert(s->chk == s->pos);

    size_t curr = s->pos;
    CF_MUNCH(s, curr, cf_iscomment);

    t->comment.loc = s->loc;
    t->comment.bgn = s->pos;
    t->comment.end = curr;

    CF_ADVANCE_LOC(s, s->chk, curr);
    s->chk = s->ind = s->nam = s->pos = curr;
    s->state = cf_parse_fq;

    return CF_TK_COMMENT;
}

cf_token_type cf_parse_fq(cf_parser_state *s, cf_token *t) {
    ASSERT_STATE(s, cf_parse_fq);
    assert(s->chk == s->pos);

    if (s->pos >= s->size) {
        s->state = cf_pop_eof;
        return CF_TK_FIELD_END;

    } else if (s->contents[s->pos] == '\n') {
        size_t curr = s->pos + 1;

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else if (s->contents[s->pos] == '\r') {
        size_t curr = s->pos + 1;
        if (curr < s->size && s->contents[curr] == '\n') { curr += 1; }

        CF_ADVANCE_LOC(s, s->chk, curr);
        s->chk = s->ind = s->nam = s->pos = curr;

        s->state = cf_parse_f0;

        return CF_SKIP(s, t);

    } else {
        CF_UNEXPECTED(s, t);
    }
}
