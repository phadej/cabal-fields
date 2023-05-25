#ifndef CABALFIELDS_H
#define CABALFIELDS_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/********************************************************************
 * Source location
 ********************************************************************/

/* If this is defined, there will be virtual CF_TK_SKIP token. */
#define CF_HAS_TK_SKIP 1

/********************************************************************
 * Source location
 ********************************************************************/

/* Source location, row and column. Both start at 1, i.e. beginning of the file is at 1:1. */
typedef struct cf_srcloc_s {
    int row;
    int col;
} cf_srcloc;

/********************************************************************
 * Token definitions
 ********************************************************************/

/* Token type. */
typedef enum cf_token_type_e {
#ifdef CF_HAS_TK_SKIP
    CF_TK_SKIP      = 1,  /* Virtual skip token. No payload. */ /* TODO: remove skip token */
#endif
    CF_TK_UNEXPECTED_CHAR          = -1, /* Error: unexpected character. */
    CF_TK_INCONSISTENT_INDENTATION = -2, /* Error: inconsistent indentation. */
    CF_TK_STACKOVERFLOW            = -3, /* Error: stack overflow. */
    CF_TK_EOF                      = 0,  /* End-of-file */
    CF_TK_COMMENT                  = 2,  /* Comment. */
    CF_TK_FIELD                    = 3,  /* Field start */
    CF_TK_FIELDLINE                = 4,  /* Field line */
    CF_TK_SECTION                  = 5,  /* Section */
    CF_TK_FIELD_END                = 6,  /* End of a field */
    CF_TK_SECTION_END              = 7,  /* End of a section */
} cf_token_type;

/* Token payload. Needs cf_token_type to disambiguate. */
typedef union cf_token_u {
    /* end token, with end position */
    struct {
        cf_srcloc loc;
        size_t pos;
    } eof;

    /* error, unexpected character */
    struct {
        cf_srcloc loc;
        const char *state; /* state name */
        uint8_t c;         /* unexpecter character */
    } error_uc;

    /* error, inconsistent indentation */
    struct {
        cf_srcloc loc;
    } error_ii;

    /* error, stack overflow */
    struct {
        cf_srcloc loc;
    } error_so;

    /* comment. */
    struct {
        cf_srcloc loc; /* Source location right after -- */
        size_t bgn;   /* comment payload range, [bgn, end), bgn <= end. */
        size_t end;
    } comment;

    /* section. */
    struct {
        cf_srcloc name_loc; /* Source location at the section name. */
        size_t name_bgn; 
        size_t name_end;

        cf_srcloc args_loc; /* Source location at the section arguments start */
        size_t args_bgn;
        size_t args_end;
    } section;

    /* field */
    struct {
        cf_srcloc name_loc; /* Source location at the section name. */
        size_t name_bgn; 
        size_t name_end;

        cf_srcloc colon_loc; /* Source location of the colon. */
    } field;

    /* fieldline */
    struct {
        cf_srcloc line_loc; /* Source location at the field line. */
        size_t line_bgn; 
        size_t line_end;
    } fieldline;

} cf_token;

/********************************************************************
 * Parser (state)
 ********************************************************************/

/* Full parser state. Treat this type as an opaque struct. */
typedef struct cf_parser_state_s cf_parser_state;

/* Reset (or initialize) parser state with new data contents and their size. */
void cf_reset(cf_parser_state *s, const uint8_t *contents, size_t size);

/* Get next token token. */
cf_token_type cf_next(cf_parser_state *s, cf_token *t);

/********************************************************************
 * Printing & debugging helpers
 ********************************************************************/

/* Splice contents substring. Result is malloc'd and '\0' terminated. */
char *cf_splice(cf_parser_state *s, size_t bgn, size_t end);

/* Convert cf_token_type to a string. */
const char *cf_token_type_str(cf_token_type t);

/* Print token contents (to stdout). */
int cf_print_token(FILE *fp, cf_token_type e, cf_token *t);

/* Print token contents (to stdout).
   This variant takes parser state so it can print token contents (not only offsets).
*/
int cf_print_token_ex(FILE *fp, cf_parser_state *s, cf_token_type e, cf_token *t);

/* Print full parser state. */
int cf_print_parser_state(FILE *fp, cf_parser_state *s);

/********************************************************************
 * Protected definitions. (You should not need to look further)
 ********************************************************************/

/* Parser state enumeration. Represent by an actual procedure function pointer. */
typedef cf_token_type (*cf_parser)(cf_parser_state *s, cf_token *t);

/* Maximum stack size.
   This constant is somewhat arbitrary.
   The current value makes sizeof(cf_parser_state) = 512 on 64bit systems.
*/
#define CF_MAX_STACK_SIZE 109

/* Full parser state.

Invariants:

* size is (less or) equal to size of contents.
* chk <= ind <= nam <= pos <= size;
* stack_size <= CF_MAX_STACK_SIZE;

*/
struct cf_parser_state_s {
	const uint8_t *contents;       /* input contents */
	size_t size;                   /* input size */
    cf_srcloc loc;                 /* input location (for humans), at chk. */
	size_t chk;                    /* input position, "checkpoint" before lexer indent and/or name */
	size_t ind;                    /* position after indent */
	size_t nam;                    /* position after name */
	size_t pos;                    /* input position (skipping whitespace) */
	cf_parser state;               /* current state, represented by a function pointer */
	size_t stack_size;             /* stack size */
	int stack[CF_MAX_STACK_SIZE];  /* stack contents */
};

#endif
