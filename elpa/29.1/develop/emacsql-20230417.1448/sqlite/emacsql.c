/* This is free and unencumbered software released into the public domain.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sqlite3.h"

#define TRUE 1
#define FALSE 0

char* escape(const char *message) {
    int i, count = 0, length_orig = strlen(message);
    for (i = 0; i < length_orig; i++) {
        if (strchr("\"\\", message[i])) {
            count++;
        }
    }
    char *copy = malloc(length_orig + count + 1);
    char *p = copy;
    while (*message) {
        if (strchr("\"\\", *message)) {
            *p = '\\';
            p++;
        }
        *p = *message;
        message++;
        p++;
    }
    *p = '\0';
    return copy;
}

void send_error(int code, const char *message) {
    char *escaped = escape(message);
    printf("error %d \"%s\"\n", code, escaped);
    free(escaped);
}

typedef struct {
    char *buffer;
    size_t size;
} buffer;

buffer* buffer_create() {
    buffer *buffer = malloc(sizeof(*buffer));
    buffer->size = 4096;
    buffer->buffer = malloc(buffer->size * sizeof(char));
    return buffer;
}

int buffer_grow(buffer *buffer) {
    unsigned factor = 2;
    char *newbuffer = realloc(buffer->buffer, buffer->size * factor);
    if (newbuffer == NULL) {
        return FALSE;
    }
    buffer->buffer = newbuffer;
    buffer->size *= factor;
    return TRUE;
}

int buffer_read(buffer *buffer, size_t count) {
    while (buffer->size < count + 1) {
        if (buffer_grow(buffer) == FALSE) {
            return FALSE;
        }
    }
    size_t in = fread((void *) buffer->buffer, 1, count, stdin);
    buffer->buffer[count] = '\0';
    return in == count;
}

void buffer_free(buffer *buffer) {
    free(buffer->buffer);
    free(buffer);
}

int main(int argc, char **argv) {
    char *file = NULL;
    if (argc != 2) {
        fprintf(stderr,
                "error: require exactly one argument, the DB filename\n");
        exit(EXIT_FAILURE);
    } else {
        file = argv[1];
    }

    /* On Windows stderr is not always unbuffered. */
#if defined(_WIN32) || defined(WIN32) || defined(__MINGW32__)
    setvbuf(stderr, NULL, _IONBF, 0);
#endif

    sqlite3* db = NULL;
    if (sqlite3_initialize() != SQLITE_OK) {
        fprintf(stderr, "error: failed to initialize sqlite\n");
        exit(EXIT_FAILURE);
    }
    int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    if (sqlite3_open_v2(file, &db, flags, NULL) != SQLITE_OK) {
        fprintf(stderr, "error: failed to open %s\n", file);
        exit(EXIT_FAILURE);
    }

    buffer *input = buffer_create();
    while (TRUE) {
        printf("#\n");
        fflush(stdout);

        /* Gather input from Emacs. */
        unsigned length;
        int result = scanf("%u ", &length);
        if (result == EOF) {
            break;
        } else if (result != 1) {
            send_error(SQLITE_ERROR, "middleware parsing error");
            break;  /* stream out of sync: quit program */
        }
        if (!buffer_read(input, length)) {
            send_error(SQLITE_NOMEM, "middleware out of memory");
            continue;
        }

        /* Parse SQL statement. */
        sqlite3_stmt *stmt = NULL;
        result = sqlite3_prepare_v2(db, input->buffer, length, &stmt, NULL);
        if (result != SQLITE_OK) {
            send_error(sqlite3_errcode(db), sqlite3_errmsg(db));
            continue;
        }

        /* Print out rows. */
        int first = TRUE, ncolumns = sqlite3_column_count(stmt);
        printf("(");
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            if (first) {
                printf("(");
                first = FALSE;
            } else {
                printf("\n (");
            }
            int i;
            for (i = 0; i < ncolumns; i++) {
                if (i > 0) {
                    printf(" ");
                }
                int type = sqlite3_column_type(stmt, i);
                switch (type) {
                case SQLITE_INTEGER:
                    printf("%lld", sqlite3_column_int64(stmt, i));
                    break;
                case SQLITE_FLOAT:
                    printf("%f", sqlite3_column_double(stmt, i));
                    break;
                case SQLITE_NULL:
                    printf("nil");
                    break;
                case SQLITE_TEXT:
                    fwrite(sqlite3_column_text(stmt, i), 1,
                           sqlite3_column_bytes(stmt, i), stdout);
                    break;
                case SQLITE_BLOB:
                    printf("nil");
                    break;
                }
            }
            printf(")");
        }
        printf(")\n");
        if (sqlite3_finalize(stmt) != SQLITE_OK) {
            /* Despite any error code, the statement is still freed.
             * http://stackoverflow.com/a/8391872
             */
            send_error(sqlite3_errcode(db), sqlite3_errmsg(db));
        } else {
            printf("success\n");
        }
    }
    buffer_free(input);

    sqlite3_close(db);
    sqlite3_shutdown();
    return EXIT_SUCCESS;
}
