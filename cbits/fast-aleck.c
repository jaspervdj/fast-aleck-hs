#include <fast-aleck/fast-aleck.h>

/* Wraps the fast_aleck function to take a pointer to the config instead of an
 * actual value. */
char *fast_aleck_(fast_aleck_config *config, char *in, size_t in_size) {
    return fast_aleck(*config, in, in_size);
}
