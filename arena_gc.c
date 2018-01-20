/*

  It would be really neat to compile to webassembly,
  but that requires some kind of GC.

  Here's one idea for how to keep the GC super simple:
  use explicit arenas!

  - To allocate, you just bump a pointer in the "current arena"
  - You can wrap any expression in a special (with-arena _) form,
  .   which creates a new arena for the evaluation of that expression.
  - When (with-arena _) returns, you just deep-copy the result into the parent arena,
  .   then discard the child arena.
  - Good candidates for (with-arena _) might be:
  .   - a single frame of a video game
  .   - a single event handler in an application
  .   - a single compiler pass

  This scheme should also be pretty easy to profile.
  You just record the current area->nextfree at every function call and return;
  the difference tells you how much allocation that function was responsible for.
  Calls with a lot of allocation might be good candidates for getting their own arena.
  

 */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

struct arena {
    struct arena *parent; // nullable
    char *end;
    char *nextfree;
    char data[];
};

struct arena *new_arena(struct arena *parent, size_t bytes) {
    struct arena *a = malloc(sizeof(struct arena) + bytes);
    a->parent = parent; // nullable
    a->nextfree = &a->data[0];
    a->end = &a->data[bytes];
    return a;
}
void free_arena(struct arena *arena) {
    // cover the arena with garbage
    for (char *p = arena->data; p < arena->end; ++p) {
	*p = -1;
    }
    
    free(arena);
}

// arenas don't know the structure of their contents;
// they just let you grab another handful of consecutive bytes.
void *arena_alloc(struct arena *a, size_t bytes) {
    void *result = a->nextfree;
    a->nextfree += bytes;
    assert(a->nextfree < a->end);
    return result;
}


struct cell {
    struct cell *car;
    struct cell *cdr;
};
struct cell *cons(struct arena *a, struct cell *car, struct cell *cdr) {
    struct cell* c = arena_alloc(a, sizeof(struct cell));
    c->car = car;
    c->cdr = cdr;
    return c;
}


// note we have to return a new pointer, because the object moved into the new arena.
struct cell *copy_collect(struct arena *dest, struct arena *source, struct cell *root) {
    // base case: the object is actually an immediate value
    if (!root) {
	return root;

	// TODO there should be another case:
	// if root is
	//  - already in dest arena
	//  - already in some ancestor of dest arena
	// then we don't want to copy it again.
    } else {
	// inductive case: recur on the children, then do the root
	struct cell *new_car = copy_collect(dest, source, root->car);
	struct cell *new_cdr = copy_collect(dest, source, root->cdr);
	return cons(dest, new_car, new_cdr);
    }
}

int length(struct cell *lst) {
    int n = 0;
    while (lst) {
	n++;
	lst = lst->cdr;
    }
    return n;
}

struct cell *build(struct arena *a, int n) {
    struct cell *c = 0;
    while (n-->0) {
	c = cons(a, 0, c);
    }
    return c;
}

struct cell *drop(struct cell *lst, int n) {
    while (n-->0) {
	lst = lst->cdr;
    }
    return lst;
}

int main() {
    // start with an outer arena
    struct arena *outer = new_arena(NULL, 9000);


    // allocate stuff in an inner arena
    struct arena *inner = new_arena(outer, 9000);

    struct cell *c = build(inner, 150);
    printf("%p %d\n", c, length(c));

    c = drop(c, 60);
    printf("%p %d\n", c, length(c));

    c = copy_collect(outer, inner, c);
    printf("%p %d\n", c, length(c));

    free_arena(inner);
    printf("%p %d\n", c, length(c));
    
}
