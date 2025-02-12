/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Rts.h"
#include "Hash.h"
#include "linker/M32Alloc.h"

#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif

void printLoadedObjects(void);

#include "BeginPrivate.h"

/* Which object file format are we targeting? */
#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
|| defined(linux_android_HOST_OS) \
|| defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) \
|| defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) \
|| defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
#  define OBJFORMAT_ELF
#elif defined(mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
#elif defined(darwin_HOST_OS) || defined(ios_HOST_OS)
#  define OBJFORMAT_MACHO
#endif

typedef void SymbolAddr;
typedef char SymbolName;
typedef struct _ObjectCode ObjectCode;
typedef struct _Section    Section;

#if defined(OBJFORMAT_ELF)
#  include "linker/ElfTypes.h"
#elif defined(OBJFORMAT_PEi386)
#  include "linker/PEi386Types.h"
#elif defined(OBJFORMAT_MACHO)
#  include "linker/MachOTypes.h"
#else
#  error "Unknown OBJECT_FORMAT for HOST_OS"
#endif


/* Hold extended information about a symbol in case we need to resolve it at a
   late stage.  */
typedef struct _Symbol
{
    SymbolName *name;
    SymbolAddr *addr;
} Symbol_t;

typedef struct NativeCodeRange_ {
  void *start, *end;

  /* Allow a chain of these things */
  struct NativeCodeRange_ *next;
} NativeCodeRange;

/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
   See Note [BFD import library].
*/
typedef
   enum { /* Section is code or readonly. e.g. .text or .r(o)data.  */
          SECTIONKIND_CODE_OR_RODATA,
          /* Section contains read/write data. e.g. .data.  */
          SECTIONKIND_RWDATA,
          /* Static initializer section. e.g. .ctors.  */
          SECTIONKIND_INIT_ARRAY,
          /* Static finalizer section. e.g. .dtors.  */
          SECTIONKIND_FINIT_ARRAY,
          /* We don't know what the section is and don't care.  */
          SECTIONKIND_OTHER,
          /* Section contains debug information. e.g. .debug$.  */
          SECTIONKIND_DEBUG,
          /* Section contains exception table. e.g. .pdata.  */
          SECTIONKIND_EXCEPTION_TABLE,
          /* Section contains unwind info. e.g. .xdata.  */
          SECTIONKIND_EXCEPTION_UNWIND,
          /* Section belongs to an import section group. e.g. .idata$.  */
          SECTIONKIND_IMPORT,
          /* Section defines an import library entry, e.g. idata$7.  */
          SECTIONKIND_IMPORT_LIBRARY,
          SECTIONKIND_NOINFOAVAIL
        }
   SectionKind;

typedef
   enum { SECTION_NOMEM,
          SECTION_M32,
          SECTION_MMAP,
          SECTION_MALLOC
        }
   SectionAlloc;

/* Indicates a desired memory protection for pages within a segment. Defined as
 * enum since it's more explicit and look nicer in a debugger.
 *
 * Can be used directly as a substitution for a combination of PROT_X flags on
 * POSIX systems.
 */
typedef enum {
#if RTS_LINKER_USE_MMAP
    SEGMENT_PROT_RO  = PROT_READ,
    SEGMENT_PROT_RX  = PROT_READ | PROT_EXEC,
    SEGMENT_PROT_RWO = PROT_READ | PROT_WRITE,
#else
    SEGMENT_PROT_RO,
    SEGMENT_PROT_RX,
    SEGMENT_PROT_RWO,
#endif
} SegmentProt;

/*
 * Note [No typedefs for customizable types]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Some pointer-to-struct types are defined opaquely
 * first, and customized later to architecture/ABI-specific
 * instantiations. Having the usual
 *   typedef struct _Foo {...} Foo;
 * wrappers is hard to get right with older versions of GCC,
 * so just have a
 *   struct Foo {...};
 * and always refer to it with the 'struct' qualifier.
 */

struct _Section {
  void*    start;              /* actual start of section in memory */
  StgWord  size;               /* actual size of section in memory */
  SectionKind kind;
  SectionAlloc alloc;

  /*
   * The following fields are relevant for SECTION_MMAP sections only
   */
  StgWord mapped_offset;      /* offset from the image of mapped_start */
  void* mapped_start;         /* start of mmap() block */
  StgWord mapped_size;        /* size of mmap() block */

  /* A customizable type to augment the Section type.
   * See Note [No typedefs for customizable types]
   */
  struct SectionFormatInfo* info;
};

typedef
   struct _ProddableBlock {
      void* start;
      int   size;
      struct _ProddableBlock* next;
   }
   ProddableBlock;

typedef struct _Segment {
    void *start;                /* page aligned start address of a segment */
    size_t size;                /* page rounded size of a segment */
    SegmentProt prot;           /* mem protection to set after all symbols were
                                 * resolved */

    int *sections_idx;          /* an array of section indexes assigned to this segment */
    int n_sections;
} Segment;

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)
#define NEED_SYMBOL_EXTRAS 1
#endif

/*
 * We use the m32 allocator for symbol extras on Windows and other mmap-using
 * platforms.
 */
#if RTS_LINKER_USE_MMAP || defined(mingw32_HOST_ARCH)
#define NEED_M32 1
#endif

/* Jump Islands are sniplets of machine code required for relative
 * address relocations on the PowerPC, x86_64 and ARM.
 */
typedef struct {
#if defined(powerpc_HOST_ARCH)
    struct {
        short lis_r12, hi_addr;
        short ori_r12_r12, lo_addr;
        long mtctr_r12;
        long bctr;
    } jumpIsland;
#elif defined(x86_64_HOST_ARCH)
    uint64_t    addr;
    // See Note [TLSGD relocation] in elf_tlsgd.c
    uint8_t     jumpIsland[8];
#elif defined(arm_HOST_ARCH)
    uint8_t     jumpIsland[16];
#endif
} SymbolExtra;

typedef enum {
    /* Objects that were loaded by this linker */
    STATIC_OBJECT,

    /* Objects that were loaded by dlopen */
    DYNAMIC_OBJECT,
} ObjectType;

/* Top-level structure for an object module.  One of these is allocated
 * for each object file in use.
 */
struct _ObjectCode {
    OStatus    status;
    pathchar  *fileName;
    int        fileSize;     /* also mapped image size when using mmap() */
    char*      formatName;   /* e.g. "ELF32", "DLL", "COFF", etc. */
    ObjectType type;         /* who loaded this object? */

    /* If this object is a member of an archive, archiveMemberName is
     * like "libarchive.a(object.o)". Otherwise it's NULL.
     */
    pathchar*      archiveMemberName;

    /* An array containing ptrs to all the symbol names copied from
       this object into the global symbol hash table.  This is so that
       we know which parts of the latter mapping to nuke when this
       object is removed from the system. */
    Symbol_t *symbols;
    int    n_symbols;

    /* ptr to mem containing the object file image */
    char*      image;

    /* A customizable type, that formats can use to augment ObjectCode
     * See Note [No typedefs for customizable types]
     */
    struct ObjectCodeFormatInfo* info;

    /* non-zero if the object file was mmap'd, otherwise malloc'd */
    int        imageMapped;

    /* record by how much image has been deliberately misaligned
       after allocation, so that we can use realloc */
    int        misalignment;

    /* The section-kind entries for this object module. An array. */
    int n_sections;
    Section* sections;

    int n_segments;
    Segment *segments;

    //
    // Garbage collection fields
    //

    // Next object in `objects` list
    struct _ObjectCode *next;

    // Previous object in `objects` list
    struct _ObjectCode *prev;

    // Next object in `loaded_objects` list
    struct _ObjectCode *next_loaded_object;

    // Mark bit
    StgWord mark;

    // Set of dependencies (ObjectCode*) of the object file. Traverse
    // dependencies using `iterHashTable`.
    //
    // New entries are added as we resolve symbols in an object file, in
    // `lookupDependentSymbol`. When an object file uses multiple symbols from
    // another object file we add the dependent multiple times, so we use a
    // `HashTable` here rather than a list/array to avoid copies.
    //
    // Used when unloading object files. See Note [Object unloading] in
    // CheckUnload.c.
    HashSet *dependencies;

    //
    // End of garbage collection fields
    //

    /* SANITY CHECK ONLY: a list of the only memory regions which may
       safely be prodded during relocation.  Any attempt to prod
       outside one of these is an error in the linker. */
    ProddableBlock* proddables;

#if defined(NEED_SYMBOL_EXTRAS)
    SymbolExtra    *symbol_extras;
    unsigned long   first_symbol_extra;
    unsigned long   n_symbol_extras;
#endif
    /* Additional memory that is preallocated and contiguous with image
       which can be used to relocate bss sections. */
    char* bssBegin;
    char* bssEnd;

    /* a list of all ForeignExportsLists owned by this object */
    struct ForeignExportsList *foreign_exports;

    /* Holds the list of symbols in the .o file which
       require extra information.*/
    StrHashTable *extraInfos;

#if defined(NEED_M32)
    /* The m32 allocators used for allocating small sections and symbol extras
     * during loading. We have two: one for (writeable) data and one for
     * (read-only/executable) code. */
    m32_allocator *rw_m32, *rx_m32;
#endif

    /*
     * The following are only valid if .type == DYNAMIC_OBJECT
     */

    /* handle returned from dlopen */
    void *dlopen_handle;

    /* virtual memory ranges of loaded code */
    NativeCodeRange *nc_ranges;
};

#define OC_INFORMATIVE_FILENAME(OC)             \
    ( (OC)->archiveMemberName ?                 \
      (OC)->archiveMemberName :                 \
      (OC)->fileName                            \
    )


#if defined(THREADED_RTS)
extern Mutex linker_mutex;

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
extern Mutex dl_mutex;
#endif
#endif

/* Type of the initializer */
typedef void (*init_t) (int argc, char **argv, char **env);

typedef enum _SymStrength {
    STRENGTH_NORMAL,
    STRENGTH_WEAK,
    STRENGTH_STRONG,
} SymStrength;

/* SymbolInfo tracks a symbol's address, the object code from which
   it originated, and whether or not it's weak.

   RtsSymbolInfo is used to track the state of the symbols currently
   loaded or to be loaded by the Linker.

   Where the information in the `ObjectCode` is used to track the
   original status of the symbol inside the `ObjectCode`.

   A weak symbol that has been used will still be marked as weak
   in the `ObjectCode` but in the `RtsSymbolInfo` it won't be.
*/
typedef struct _RtsSymbolInfo {
    SymbolAddr* value;
    ObjectCode *owner;
    SymStrength strength;
} RtsSymbolInfo;

void exitLinker( void );

void freeObjectCode (ObjectCode *oc);
SymbolAddr* loadSymbol(SymbolName *lbl, RtsSymbolInfo *pinfo);

void addProddableBlock ( ObjectCode* oc, void* start, int size );
void checkProddableBlock (ObjectCode *oc, void *addr, size_t size );
void freeProddableBlocks (ObjectCode *oc);

void addSection (Section *s, SectionKind kind, SectionAlloc alloc,
                 void* start, StgWord size, StgWord mapped_offset,
                 void* mapped_start, StgWord mapped_size);

HsBool ghciLookupSymbolInfo(StrHashTable *table,
                            const SymbolName* key, RtsSymbolInfo **result);

int ghciInsertSymbolTable(
    pathchar* obj_name,
    StrHashTable *table,
    const SymbolName* key,
    SymbolAddr* data,
    SymStrength weak,
    ObjectCode *owner);

/* Lock-free version of lookupSymbol. When 'dependent' is not NULL, adds it as a
 * dependent to the owner of the symbol. */
SymbolAddr* lookupDependentSymbol (SymbolName* lbl, ObjectCode *dependent);

/* Perform TLSGD symbol lookup returning the address of the resulting GOT entry,
 * which in this case holds the module id and the symbol offset. */
StgInt64 lookupTlsgdSymbol(const char *, unsigned long, ObjectCode *);

extern StrHashTable *symhash;

pathchar*
resolveSymbolAddr (pathchar* buffer, int size,
                   SymbolAddr* symbol, uintptr_t* top);

/*************************************************
 * Various bits of configuration
 *************************************************/

/* PowerPC and ARM have relative branch instructions with only 24 bit
 * displacements and therefore need jump islands contiguous with each object
 * code module.
 */
#if defined(powerpc_HOST_ARCH)
#define SHORT_REL_BRANCH 1
#endif
#if defined(arm_HOST_ARCH)
#define SHORT_REL_BRANCH 1
#endif

#if (RTS_LINKER_USE_MMAP && defined(SHORT_REL_BRANCH) && defined(linux_HOST_OS))
#define USE_CONTIGUOUS_MMAP 1
#else
#define USE_CONTIGUOUS_MMAP 0
#endif


HsInt isAlreadyLoaded( pathchar *path );
OStatus getObjectLoadStatus_ (pathchar *path);
HsInt loadOc( ObjectCode* oc );
ObjectCode* mkOc( ObjectType type, pathchar *path, char *image, int imageSize,
                  bool mapped, pathchar *archiveMemberName,
                  int misalignment
                  );

void initSegment(Segment *s, void *start, size_t size, SegmentProt prot, int n_sections);
void freeSegments(ObjectCode *oc);

#include "EndPrivate.h"
