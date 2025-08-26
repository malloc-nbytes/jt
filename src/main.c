#include "jt.h"

#include <forge/chooser.h>

#include <assert.h>
#include <stdio.h>

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
