#include <fast-aleck/fast-aleck.h>

/* Wraps the fast_aleck function to take a pointer to the config instead of an
 * actual value. */
inline char *fast_aleck_wrapper(fast_aleck_config *config, char *in,
        size_t in_size, size_t *ao_len) {
    return fast_aleck(*config, in, in_size, ao_len);
}
