#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <time.h>
#include "arena.h"

#define MAX(a,b) ((a) > (b) ? (a) : (b))

char *read_file_contents(const char *filename);
