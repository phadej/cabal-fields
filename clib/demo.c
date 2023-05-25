#include "cabalfields.h"
#include "readfile.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage() {
	printf("Usage: ./demo [-m | -w] input.txt\n");
}

int main(int argc, char **argv) {
	printf("sizeof(cf_parser_state) = %ld\n", sizeof(cf_parser_state));

    /* newline modifiers */
    bool windows = false;
    bool macos   = false;
    bool trail   = false;

    const char *filename = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-u") == 0) { trail = false; macos = false; windows = false; continue; }
        if (strcmp(argv[i], "-t") == 0) { trail = true;  macos = false; windows = false; continue; }
        if (strcmp(argv[i], "-m") == 0) { trail = false; macos = true;  windows = false; continue; }
        if (strcmp(argv[i], "-w") == 0) { trail = false; macos = false; windows = true;  continue; }
        if (filename == NULL) { filename = argv[i]; break; }
    }

    if (filename == NULL) {
        usage();
        return 1;
    }

	uint8_t *contents = NULL;
	size_t size = 0;

	if (readfile(filename, &contents, &size) != 0) {
		perror("readfile");
		return 1;
	}

    if (macos) {
        for (size_t i = 0; i < size; i++) {
            if (contents[i] == '\n') { contents[i] = '\r'; }
        }
    }

    if (windows) {
        size_t newlines = 0;
        for (size_t i = 0; i < size; i++) {
            if (contents[i] == '\n') { newlines += 1; }
        }

        size_t new_size = size + newlines;
        uint8_t *new_contents = malloc(new_size);

        for (size_t i = 0, j = 0; i < size; i++) {
            if (contents[i] == '\n') {
                new_contents[j++] = '\r';
                new_contents[j++] = '\n';
            } else {
                new_contents[j++] = contents[i];
            }
        }

        free(contents);
        size = new_size;
        contents = new_contents;
    }

    if (trail) {
        size_t newlines = 0;
        for (size_t i = 0; i < size; i++) {
            if (contents[i] == '\n') { newlines += 1; }
        }

        size_t new_size = size + newlines;
        uint8_t *new_contents = malloc(new_size);

        for (size_t i = 0, j = 0; i < size; i++) {
            if (contents[i] == '\n') {
                new_contents[j++] = ' ';
                new_contents[j++] = '\n';
            } else {
                new_contents[j++] = contents[i];
            }
        }

        free(contents);
        size = new_size;
        contents = new_contents;
    }

	cf_parser_state s = {0};
	cf_reset(&s, contents, size);

	while (1) {
		cf_print_parser_state(stdout, &s);

		cf_token t;
		cf_token_type e =  cf_next(&s, &t);

#ifdef CF_HAS_TK_SKIP
        if (e != CF_TK_SKIP) { cf_print_token_ex(stdout, &s, e, &t); }
#else
        cf_print_token_ex(&s, e, &t);
#endif

        /* if e is CF_TK_ERR or CF_TK_EOF */
		if (e <= 0) break;
	}

	free(contents);

	return 0;
}
