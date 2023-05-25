#ifndef READFILE_H
#define READFILE_H

#include <stddef.h>
#include <stdint.h>

int readfile(const char *filename, uint8_t **contents, size_t *size);

#endif
