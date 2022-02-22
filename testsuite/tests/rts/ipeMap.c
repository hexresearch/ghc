#include <stdlib.h>
#include <string.h>

#include "Rts.h"

void assertStringsEqual(char *s1, char *s2);
void shouldFindNothingInAnEmptyIPEMap(Capability *cap);
HaskellObj shouldFindOneIfItHasBeenRegistered(Capability *cap);
void shouldFindTwoIfTwoHaveBeenRegistered(Capability *cap, HaskellObj fortyTwo);
void shouldFindTwoFromTheSameList(Capability *cap);
void shouldFindTheLastEntryOfManyLists(Capability *cap);
void shouldDealWithAnEmptyList(Capability *cap, HaskellObj);

// This is a unit test for IPE.c, the IPE map.
// Due to the nature of IPE having static state, the test cases are not
// independent of each other!
int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    Capability *cap = rts_lock();

    shouldFindNothingInAnEmptyIPEMap(cap);
    HaskellObj fortyTwo = shouldFindOneIfItHasBeenRegistered(cap);
    shouldFindTwoIfTwoHaveBeenRegistered(cap, fortyTwo);
    shouldFindTwoFromTheSameList(cap);
    shouldFindTheLastEntryOfManyLists(cap);
    shouldDealWithAnEmptyList(cap, fortyTwo);

    rts_unlock(cap);
    hs_exit();
}

void shouldFindNothingInAnEmptyIPEMap(Capability *cap) {
    HaskellObj fortyTwo = UNTAG_CLOSURE(rts_mkInt(cap, 42));

    InfoProvEnt *result = lookupIPE(get_itbl(fortyTwo));

    if (result != NULL) {
        errorBelch("Found entry in an empty IPE map!");
        exit(1);
    }
}

HaskellObj shouldFindOneIfItHasBeenRegistered(Capability *cap) {
    HaskellObj fortyTwo = UNTAG_CLOSURE(rts_mkInt(cap, 42));

    InfoProvEnt *provEnt = malloc(sizeof(InfoProvEnt));
    provEnt->info = get_itbl(fortyTwo);
    provEnt->prov.table_name = "table_name_42";
    provEnt->prov.closure_desc = "closure_desc_42";
    provEnt->prov.ty_desc = "ty_desc_42";
    provEnt->prov.label = "label_42";
    provEnt->prov.module = "module_42";
    provEnt->prov.srcloc = "srcloc_42";

    InfoProvEnt *ipeList[] = {provEnt, NULL};

    registerInfoProvList(ipeList);
    InfoProvEnt *result = lookupIPE(get_itbl(fortyTwo));

    if (result == NULL) {
        errorBelch("Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(result->prov.table_name, "table_name_42");
    assertStringsEqual(result->prov.closure_desc, "closure_desc_42");
    assertStringsEqual(result->prov.ty_desc, "ty_desc_42");
    assertStringsEqual(result->prov.label, "label_42");
    assertStringsEqual(result->prov.module, "module_42");
    assertStringsEqual(result->prov.srcloc, "srcloc_42");

    return fortyTwo;
}

void shouldFindTwoIfTwoHaveBeenRegistered(Capability *cap,
                                          HaskellObj fortyTwo) {
  HaskellObj twentyThree = UNTAG_CLOSURE(rts_mkInt8(cap, 23));

    InfoProvEnt *provEnt = malloc(sizeof(InfoProvEnt));
    provEnt->info = get_itbl(twentyThree);
    provEnt->prov.table_name = "table_name_23";
    provEnt->prov.closure_desc = "closure_desc_23";
    provEnt->prov.ty_desc = "ty_desc_23";
    provEnt->prov.label = "label_23";
    provEnt->prov.module = "module_23";
    provEnt->prov.srcloc = "srcloc_23";

    InfoProvEnt *ipeList[] = {provEnt, NULL};

    registerInfoProvList(ipeList);

    InfoProvEnt *resultFortyTwo =
      lookupIPE(get_itbl(fortyTwo));
    InfoProvEnt *resultTwentyThree =
      lookupIPE(get_itbl(twentyThree));

    if (resultFortyTwo == NULL || resultTwentyThree == NULL) {
        errorBelch("Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultFortyTwo->prov.table_name, "table_name_42");
    assertStringsEqual(resultTwentyThree->prov.table_name, "table_name_23");
}

void shouldFindTwoFromTheSameList(Capability *cap) {
  HaskellObj one = UNTAG_CLOSURE(rts_mkInt16(cap, 1));

    InfoProvEnt *provEntOne = malloc(sizeof(InfoProvEnt));
    provEntOne->info = get_itbl(one);
    provEntOne->prov.table_name = "table_name_1";
    provEntOne->prov.closure_desc = "closure_desc_1";
    provEntOne->prov.ty_desc = "ty_desc_1";
    provEntOne->prov.label = "label_1";
    provEntOne->prov.module = "module_1";
    provEntOne->prov.srcloc = "srcloc_1";

    HaskellObj two = UNTAG_CLOSURE(rts_mkInt32(cap, 2));

    InfoProvEnt *provEntTwo = malloc(sizeof(InfoProvEnt));
    provEntTwo->info = get_itbl(two);
    provEntTwo->prov.table_name = "table_name_2";
    provEntTwo->prov.closure_desc = "closure_desc_2";
    provEntTwo->prov.ty_desc = "ty_desc_2";
    provEntTwo->prov.label = "label_2";
    provEntTwo->prov.module = "module_2";
    provEntTwo->prov.srcloc = "srcloc_2";

    InfoProvEnt *ipeList[] = {provEntOne, provEntTwo, NULL};

    registerInfoProvList(ipeList);

    InfoProvEnt *resultOne = lookupIPE(get_itbl(one));
    InfoProvEnt *resultTwo = lookupIPE(get_itbl(two));

    if (resultOne == NULL || resultOne == NULL) {
        errorBelch("Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultOne->prov.table_name, "table_name_1");
    assertStringsEqual(resultTwo->prov.table_name, "table_name_2");
}

void shouldFindTheLastEntryOfManyLists(Capability *cap) {
    HaskellObj three = UNTAG_CLOSURE(rts_mkInt64(cap, 3));

    InfoProvEnt *provEntThree = malloc(sizeof(InfoProvEnt));
    provEntThree->info = get_itbl(three);
    provEntThree->prov.table_name = "table_name_3";
    provEntThree->prov.closure_desc = "closure_desc_3";
    provEntThree->prov.ty_desc = "ty_desc_3";
    provEntThree->prov.label = "label_3";
    provEntThree->prov.module = "module_3";
    provEntThree->prov.srcloc = "srcloc_3";

    HaskellObj four = UNTAG_CLOSURE(rts_mkWord8(cap, 4));

    InfoProvEnt *provEntFour = malloc(sizeof(InfoProvEnt));
    provEntFour->info = get_itbl(four);
    provEntFour->prov.table_name = "table_name_4";
    provEntFour->prov.closure_desc = "closure_desc_4";
    provEntFour->prov.ty_desc = "ty_desc_4";
    provEntFour->prov.label = "label_4";
    provEntFour->prov.module = "module_4";
    provEntFour->prov.srcloc = "srcloc_4";

    InfoProvEnt *ipeListThree[] = {provEntThree, NULL};
    InfoProvEnt *ipeListFour[] = {provEntFour, NULL};

    // Force the creation of 4 IpeBufferListNodes
    for (int i = 0; i <= 126 * 3 + 1; i++) {
        registerInfoProvList(ipeListThree);
    }

    registerInfoProvList(ipeListFour);

    InfoProvEnt *resultFour = lookupIPE(get_itbl(four));

    if (resultFour == NULL) {
        errorBelch("Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultFour->prov.table_name, "table_name_4");
}

void shouldDealWithAnEmptyList(Capability *cap, HaskellObj fortyTwo) {
    InfoProvEnt *emptyIpeList[] = {NULL};

    registerInfoProvList(emptyIpeList);

    InfoProvEnt *resultFortyTwo =
        lookupIPE(get_itbl(fortyTwo));

    if (resultFortyTwo == NULL) {
        errorBelch("Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultFortyTwo->prov.table_name, "table_name_42");
}

void assertStringsEqual(char *s1, char *s2) {
    if (strcmp(s1, s2) != 0) {
        errorBelch("%s != %s", s1, s2);
        exit(1);
    }
}
