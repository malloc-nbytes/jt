#ifndef JT_H_INCLUDED
#define JT_H_INCLUDED

#include <forge/array.h>

#define JT_FILE ".jt"

typedef enum {
        STATUS_PENDING = 0,
        STATUS_REJECTED,
        STATUS_ACCEPTED,
} status;

typedef struct {
        char *title;
        char *pay;
        char *desc;
        status status;
        str_array info;
} entry;

DYN_ARRAY_TYPE(entry *, entry_array);

typedef struct {
        entry_array entries;
        int saved;
} jt_context;

entry_array read_jt_file(void);
void add_entry(jt_context *ctx);
void remove_entry(jt_context *ctx);
void edit_entry(jt_context *ctx);
void list_entries(jt_context *ctx);
void save(jt_context *ctx);

#endif // JT_H_INCLUDED
