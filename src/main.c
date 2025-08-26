#include <forge/viewer.h>
#include <forge/chooser.h>
#include <forge/rdln.h>
#include <forge/cmd.h>
#include <forge/array.h>
#include <forge/err.h>
#include <forge/viewer.h>
#include <forge/cstr.h>
#include <forge/str.h>
#include <forge/io.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>

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

const char *
status_to_cstr(status st)
{
        switch (st) {
        case STATUS_PENDING: return "PENDING";
        case STATUS_REJECTED: return "REJECTED";
        case STATUS_ACCEPTED: return "ACCEPTED";
        default: {
                forge_err_wargs("status_to_cstr(): unknown status: %d", (int)st);
        } break;
        }
        return NULL; // unreachable
}

const char *
get_jt_file(void)
{
        constexpr size_t buf_cap = 256;
        static char buf[buf_cap] = {0};
        size_t buf_len = 0;
        memset(buf, 0, sizeof(buf));

        char *home = env("HOME");
        if (!home) { return NULL; }

        assert(strlen(home)+1+strlen(JT_FILE) <= buf_cap);

        strcpy(buf, home);
        buf_len += strlen(buf);
        buf[buf_len++] = '/';
        strcat(buf, JT_FILE);

        return buf;
}

entry *
entry_create(const char *title,
             const char *pay,
             const char *desc,
             status      status,
             str_array   info)
{
        entry *e = (entry *)malloc(sizeof(entry));
        e->title = strdup(title);
        e->pay = strdup(pay);
        e->desc = strdup(desc);
        e->status = status;
        e->info = info;
        return e;
}

status
get_status_from_cstr(const char *st)
{
        if (!strcmp(st, "PENDING")) return STATUS_PENDING;
        if (!strcmp(st, "REJECTED")) return STATUS_REJECTED;
        if (!strcmp(st, "ACCEPTED")) return STATUS_ACCEPTED;
        forge_err_wargs("get_status_from_cstr(): unknown status: %s", st);
        return 0; // unreachable
}

entry_array
read_jt_file(void)
{
        const char *fp = get_jt_file();

        if (!forge_io_filepath_exists(fp)) {
                return dyn_array_empty(entry_array);
        }

        entry_array ar = dyn_array_empty(entry_array);

        char **lns = forge_io_read_file_to_lines(fp);

        const char *title = NULL, *pay = NULL, *desc = NULL, *status = NULL;
        str_array info = dyn_array_empty(str_array);

        for (size_t i = 0; lns && lns[i]; ++i) {
                if (!strcmp(lns[i], "INFO")) {
                        ++i;
                        while (lns[i] && strcmp(lns[i], "JT_INFO_DONE") != 0) {
                                dyn_array_append(info, lns[i]);
                                ++i;
                        }
                        dyn_array_append(ar, entry_create(title, pay, desc,
                                                          get_status_from_cstr(status), info));
                        dyn_array_clear(info);
                        title = pay = desc = status = NULL;
                        //++i;
                } else {
                        const char *colon = forge_cstr_first_of(lns[i], ':');
                        if (!colon) continue;

                        const char *val = colon+2;
                        size_t span_n = colon-lns[i];
                        if (!strncmp(lns[i], "TITLE", span_n)) {
                                title = val;
                        } else if (!strncmp(lns[i], "PAY", span_n)) {
                                pay = val;
                        } else if (!strncmp(lns[i], "DESCRIPTION", span_n)) {
                                desc = val;
                        } else if (!strncmp(lns[i], "STATUS", span_n)) {
                                status = val;
                        } else {
                                continue;
                        }
                }
        }

        return ar;
}

void
add_entry(jt_context *ctx)
{
        char *title = forge_rdln("Title: ");
        char *pay = forge_rdln("Pay: ");
        char *desc = forge_rdln("Description: ");
        str_array info = dyn_array_empty(str_array);

        printf("End with 'eof' or ':q'\n");
        while (1) {
                char *ln = forge_rdln("Extra Info: ");
                if (!ln) continue;
                if (!strcmp(ln, "eof") || !strcmp(ln, ":q")) {
                        free(ln);
                        break;
                }
                dyn_array_append(info, ln);
        }

        dyn_array_append(ctx->entries,
                         entry_create(title, pay, desc,
                                      STATUS_PENDING, info));

        ctx->saved = 0;
}

void
remove_entry(jt_context *ctx)
{
        forge_todo("remove");
}

void
edit_entry(jt_context *ctx)
{
        forge_todo("edit");
}

str_array
get_entry_information(const entry *entry)
{
        str_array content = dyn_array_empty(str_array);

        dyn_array_append(content, forge_cstr_builder("Title: ",       entry->title,                  NULL));
        dyn_array_append(content, forge_cstr_builder("Pay: ",         entry->pay,                    NULL));
        dyn_array_append(content, forge_cstr_builder("Description: ", entry->desc,                   NULL));
        dyn_array_append(content, forge_cstr_builder("Status: ",      status_to_cstr(entry->status), NULL));
        dyn_array_append(content, strdup("Information:"));

        for (size_t i = 0; i < entry->info.len; ++i) {
                forge_str s = forge_str_from("  ");
                forge_str_concat(&s, entry->info.data[i]);
                dyn_array_append(content, s.data);
        }

        return content;
}

void
list_entries(jt_context *ctx)
{
        str_array titles = dyn_array_empty(str_array);

        for (size_t i = 0; i < ctx->entries.len; ++i) {
                dyn_array_append(titles, ctx->entries.data[i]->title);
        }

        size_t last_row = 0;
        while (1) {
                int choice = forge_chooser("Choose an entry",
                                           (const char **)titles.data,
                                           titles.len, last_row);

                if (choice == -1) break;

                str_array content = get_entry_information(ctx->entries.data[choice]);
                forge_viewer *v = forge_viewer_alloc(content.data, content.len, 1);
                forge_viewer_display(v);
                forge_viewer_free(v);
                for (size_t i = 0; i < content.len; ++i) {
                        free(content.data[i]);
                }
                dyn_array_free(content);
                last_row = choice;
        }
        dyn_array_free(titles);
}

void
save(jt_context *ctx)
{
        const char *fp = get_jt_file();

        if (!forge_io_filepath_exists(fp)) {
                forge_io_create_file(fp, 1);
        }

        str_array lns = dyn_array_empty(str_array);
        for (size_t i = 0; i < ctx->entries.len; ++i) {
                const entry *e = ctx->entries.data[i];
                dyn_array_append(lns, forge_cstr_builder("TITLE: ",       e->title,                  NULL));
                dyn_array_append(lns, forge_cstr_builder("PAY: ",         e->pay,                    NULL));
                dyn_array_append(lns, forge_cstr_builder("DESCRIPTION: ", e->desc,                   NULL));
                dyn_array_append(lns, forge_cstr_builder("STATUS: ",      status_to_cstr(e->status), NULL));
                dyn_array_append(lns,                                     strdup("INFO"));
                for (size_t i = 0; i < e->info.len; ++i) {
                        dyn_array_append(lns, strdup(e->info.data[i]));
                }
                dyn_array_append(lns, strdup("JT_INFO_DONE"));
        }

        (void)forge_io_write_lines(fp, (const char **)lns.data, lns.len);

        for (size_t i = 0; i < lns.len; ++i) {
                free(lns.data[i]);
        }
        dyn_array_free(lns);

        ctx->saved = 1;
}

int
main(void)
{
        jt_context ctx = (jt_context) {
                .entries = read_jt_file(),
                .saved = 1,
        };

        void (*funs[])(jt_context *) = {
                add_entry,
                remove_entry,
                edit_entry,
                list_entries,
                save,
        };

        const char *choices[] = {
                "Add an Entry",
                "Remove an Entry",
                "Edit an Entry",
                "List entries",
                "Save",
        };

        constexpr size_t choices_n = sizeof(choices)/sizeof(*choices);

        while (1) {
                int choice = forge_chooser("Choose an option ['q' to quit]",
                                           choices,
                                           choices_n, 0);

                if (choice == -1) {
                        if (!ctx.saved) {
                                if (forge_chooser_yesno("You have unsaved changes, really quit?",
                                                        NULL, 0) == 1) {
                                        break;
                                }
                                continue;
                        }
                        break;
                }

                assert(choice < choices_n);
                funs[choice](&ctx);
        }

        return 0;
}
