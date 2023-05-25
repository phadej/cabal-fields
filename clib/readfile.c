#include "readfile.h"

#include <stdio.h>
#include <stdlib.h>

int readfile(const char *filename, uint8_t **contents, size_t *size) {
	uint8_t *buffer = *contents;

	if (buffer == NULL) {
		if (*size == 0) { *size = 32768; }
		buffer = malloc(*size);
	}

	FILE *fp = fopen(filename, "r");
	if (fp == NULL) {
		return 1;
	}

	size_t r = fread(buffer, 1, *size, fp);

	if (ferror(fp) != 0) {
		free(buffer);
		return 0;
	}
	*size = r;
	buffer[r] = '\0';

	*contents = buffer;

    fclose(fp);

	return 0;
}
