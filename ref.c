/* File: ./lec15.adj.snax.c */
/*
   opt_id=true
   opt_reuse=true
 */

#include <stdio.h>     /* printf() */
#include <stdlib.h>    /* malloc() */
#include <string.h>    /* memcpy() */
#include <sys/mman.h>  /* mmap() */
#include <locale.h>    /* %' directive for format string */

typedef enum tag {
  b0, b1, e,                  /* type bin = +{'b0 : down[k] bin[k], 'b1 : down[k] bin[k], 'e : 1} */
  nil, cons,                  /* type list = +{'nil : 1, 'cons : down[k] bin[k] * down[m] list[m k]} */
  _dummy_
} tag;

typedef union value* addr;
typedef union value {
  tag  tag;                                       /* +{l:Al}, &{l:Al} */
  addr ptr;                                       /* (heap) address */
} value;

typedef struct closure* closure;
struct closure {
  void(*fun)(addr arg, addr env);                 /* function pointer */
  addr env;                                       /* environment */
};

typedef struct bin* list_1;

typedef struct list* list_2;

struct list_3 {
  list_1 pi1;
  list_2 pi2;
};
typedef struct list_3 list_3;

struct list_0 {};
typedef struct list_0 list_0;

struct bin_2 {};
typedef struct bin_2 bin_2;

typedef struct bin* bin_1;

typedef struct bin* bin_0;

struct bin {
  tag tag;
  union {
    bin_0 b0;
    bin_1 b1;
    bin_2 e;
  };
};
typedef struct bin bin;

struct list {
  tag tag;
  union {
    list_0 nil;
    list_3 cons;
  };
};
typedef struct list list;



static void* heap;
static unsigned long alloc_count;
static unsigned long alloc_size;

void init_heap(size_t total_size) {
  heap = mmap(NULL, total_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if (heap == MAP_FAILED) {
    printf("mmap failed");
    exit(EXIT_FAILURE);
  }
}

addr alloc(int n) {
  void* prev = heap;
  int n8 = n; /* or: (n + (8-1)) & -8 */
  heap += n8;
  alloc_count++;
  alloc_size += n;
  return (addr)prev;
}

void invoke_closure_fun (closure a, addr b, addr c) {
  addr arg = alloca(2 * sizeof(value));  /* allocate on stack */
  arg->ptr = b;
  (arg+1)->ptr = c;
  (a->fun)(arg, a->env);
}

void invoke_closure_susp (closure a, addr b) {
  addr arg = alloca(1 * sizeof(value));  /* allocate on stack */
  arg->ptr = b;
  (a->fun)(arg, a->env);
}

void invoke_closure_record (closure a, tag k, addr b) {
  addr arg = alloca(2 * sizeof(value));  /* allocate on stack */
  arg->tag = k;
  (arg+1)->ptr = b;
  (a->fun)(arg, a->env);
}

void print__list$3(int depth, list_3* $0);
void print__list$2(int depth, list_2* $0);
void print__list$1(int depth, list_1* $0);
void print__list$0(int depth, list_0* $0);
void print__bin$2(int depth, bin_2* $0);
void print__bin$1(int depth, bin_1* $0);
void print__bin$0(int depth, bin_0* $0);
void print_bin(int depth, bin* $0);
void print_list(int depth, list* $0);


void print__list$3(int depth, list_3* $0) {
  if (depth == 0) { printf("..."); return; }
  printf("(");
  print__list$1(depth-1, &((*$0).pi1));
  printf(", ");
  print__list$2(depth-1, &((*$0).pi2));
  printf(")");
}

void print__list$2(int depth, list_2* $0) {
  if (depth == 0) { printf("..."); return; }
  printf("<");
  print_list(depth-1, *$0);
  printf(">");
}

void print__list$1(int depth, list_1* $0) {
  if (depth == 0) { printf("..."); return; }
  printf("<");
  print_bin(depth-1, *$0);
  printf(">");
}

void print__list$0(int depth, list_0* $0) {
  printf("()");
}

void print__bin$2(int depth, bin_2* $0) {
  printf("()");
}

void print__bin$1(int depth, bin_1* $0) {
  if (depth == 0) { printf("..."); return; }
  printf("<");
  print_bin(depth-1, *$0);
  printf(">");
}

void print__bin$0(int depth, bin_0* $0) {
  if (depth == 0) { printf("..."); return; }
  printf("<");
  print_bin(depth-1, *$0);
  printf(">");
}

void print_bin(int depth, bin* $0) {
  if (depth == 0) { printf("..."); return; }
  switch ((*$0).tag) {
  case b0:
    printf("'b0 ");
    print__bin$0(depth-1, &((*$0).b0));
    break;
  case b1:
    printf("'b1 ");
    print__bin$1(depth-1, &((*$0).b1));
    break;
  case e:
    printf("'e ");
    print__bin$2(depth-1, &((*$0).e));
    break;
  default: exit(EXIT_FAILURE);
  }
}

void print_list(int depth, list* $0) {
  if (depth == 0) { printf("..."); return; }
  switch ((*$0).tag) {
  case nil:
    printf("'nil ");
    print__list$0(depth-1, &((*$0).nil));
    break;
  case cons:
    printf("'cons ");
    print__list$3(depth-1, &((*$0).cons));
    break;
  default: exit(EXIT_FAILURE);
  }
}



void reverse$0(list* $0, list* l, list* acc);
void zero$0(bin* $0);
void one$0(bin* $0);
void two$0(bin* $0);
void main$0(list* $0);


void reverse$0(list* $0, list* l, list* acc) {    /* proc reverse/0 ($0:list[L L]) (l:list[L L]) (acc:list[L L]) */
  switch ((*l).tag) {                             /* read l */
    case nil: {                                   /* | nil(_) => */
      /* silent read */                           /* read l.nil () => */
      memcpy($0, acc, sizeof(list));              /* id $0 acc : list[L L] */
      break;
    }
    case cons: {                                  /* | cons(_) => */
      /* silent read */                           /* read l.cons (_, _) => */
      typeof((*l).cons.pi1) $1 = (*l).cons.pi1;   /* read l.cons.pi1 <$1> => */
      typeof((*l).cons.pi2) $2 = (*l).cons.pi2;   /* read l.cons.pi2 <$2> => */
      list* $5 = l;                               /* reuse $5 = l : list[L L] */
      (*$5).cons.pi1 = $1;                        /* write $5.cons.pi1 <$1> */
      (*$5).cons.pi2 = acc;                       /* write $5.cons.pi2 <acc> */
      /* silent write */                          /* write $5.cons (_, _) */
      (*$5).tag = cons;                           /* write $5 cons(_) */
      reverse$0($0, $2, $5);                      /* call reverse/0 $0 $2 $5 */
      break;
    }
    default: exit(EXIT_FAILURE);
  }
}

void zero$0(bin* $0) {                            /* proc zero/0 ($0:bin[L])  */
  /* silent write */                              /* write $0.e () */
  (*$0).tag = e;                                  /* write $0 e(_) */
}

void one$0(bin* $0) {                             /* proc one/0 ($0:bin[L])  */
  bin* $1 = (bin*)alloc(sizeof(bin));             /* alloc $1 : bin[L] */
  zero$0($1);                                     /* call zero/0 $1 */
  (*$0).b1 = $1;                                  /* write $0.b1 <$1> */
  (*$0).tag = b1;                                 /* write $0 b1(_) */
}

void two$0(bin* $0) {                             /* proc two/0 ($0:bin[L])  */
  bin* $1 = (bin*)alloc(sizeof(bin));             /* alloc $1 : bin[L] */
  one$0($1);                                      /* call one/0 $1 */
  (*$0).b0 = $1;                                  /* write $0.b0 <$1> */
  (*$0).tag = b0;                                 /* write $0 b0(_) */
}

void main$0(list* $0) {                           /* proc main/0 ($0:list[L L])  */
  list* $2 = (list*)alloc(sizeof(list));          /* alloc $2 : list[L L] */
  bin* $4 = (bin*)alloc(sizeof(bin));             /* alloc $4 : bin[L] */
  zero$0($4);                                     /* call zero/0 $4 */
  (*$2).cons.pi1 = $4;                            /* write $2.cons.pi1 <$4> */
  list* $6 = (list*)alloc(sizeof(list));          /* alloc $6 : list[L L] */
  bin* $7 = (bin*)alloc(sizeof(bin));             /* alloc $7 : bin[L] */
  one$0($7);                                      /* call one/0 $7 */
  (*$6).cons.pi1 = $7;                            /* write $6.cons.pi1 <$7> */
  list* $9 = (list*)alloc(sizeof(list));          /* alloc $9 : list[L L] */
  bin* $10 = (bin*)alloc(sizeof(bin));            /* alloc $10 : bin[L] */
  two$0($10);                                     /* call two/0 $10 */
  (*$9).cons.pi1 = $10;                           /* write $9.cons.pi1 <$10> */
  list* $12 = (list*)alloc(sizeof(list));         /* alloc $12 : list[L L] */
  /* silent write */                              /* write $12.nil () */
  (*$12).tag = nil;                               /* write $12 nil(_) */
  (*$9).cons.pi2 = $12;                           /* write $9.cons.pi2 <$12> */
  /* silent write */                              /* write $9.cons (_, _) */
  (*$9).tag = cons;                               /* write $9 cons(_) */
  (*$6).cons.pi2 = $9;                            /* write $6.cons.pi2 <$9> */
  /* silent write */                              /* write $6.cons (_, _) */
  (*$6).tag = cons;                               /* write $6 cons(_) */
  (*$2).cons.pi2 = $6;                            /* write $2.cons.pi2 <$6> */
  /* silent write */                              /* write $2.cons (_, _) */
  (*$2).tag = cons;                               /* write $2 cons(_) */
  list* $3 = (list*)alloc(sizeof(list));          /* alloc $3 : list[L L] */
  /* silent write */                              /* write $3.nil () */
  (*$3).tag = nil;                                /* write $3 nil(_) */
  reverse$0($0, $2, $3);                          /* call reverse/0 $0 $2 $3 */
}



int main() {
  setlocale(LC_NUMERIC, "");
  init_heap(0x80000000);
  list* $0 = (list*)alloc(sizeof(list));
  main$0($0);                                     /* call main/0 $0 */
  print_list(20, $0);
  printf("\n");
  fprintf(stderr,"%% Allocated %'lu objects\n", alloc_count);
  fprintf(stderr,"%% Total size = %'lu bytes\n", alloc_size);
  return 0;
}
