heapBegin 0;
heapEnd 0;

/*
 * Allocates the given number of quads (64 bit values)
 * Inefficient algorithm... but it's fine for now
 */
malloc(count) {
    extrn heapBegin, heapEnd;
    if (heapBegin == 0) {
        heapBegin = syscall(12, 0);
        heapEnd = syscall(12, heapBegin + 077777);
        /* Slot header: (size << 1) + occupiedBit  */
        /* size=0 and occupied=0 indicate the end of the chunk list */
        heapBegin[0] = 0;
    }

    auto ptr, byteCount;
    /* Round up to the nearest 512 byte chunk */
    byteCount = (count * 8 + 0777) & ~0777;

    ptr = heapBegin;
    /* Iterate the ptr until we hit an unoccupied slot of the right size */
    /* while (((ptr[0] >> 1) != 0) & ((ptr[0] & 1) != 0) & ((ptr[0] >> 1) != byteCount)) { */
    while (1) {
        if ((ptr[0] >> 1) == 0) break;

        /* Unoccupied & of the right size*/
        if ((ptr[0] & 1) == 0)
            if ((ptr[0] >> 1) == byteCount)
                break;
        ptr =+ (ptr[0] >> 1) + 8;
    }

    /* If the count is 0, we're at the end */
    if ((ptr[0] >> 1) == 0) {
        if (ptr + byteCount + 8 >= heapEnd) {
            /* Increase the heap size in increments of 32KiB */
            heapEnd = syscall(12, (ptr + byteCount + 8 + 077777) & ~077777);
        }
        /* Set a new null terminator */
        *(ptr + byteCount + 8) = 0;
        ptr[0] = (byteCount << 1) + 1;
        return(ptr + 8);
    } else {
        /* We found a cozy spot of unallocated space that we can reuse */
        /* It just picks the first "big enough" slot. The packing is wasteful */

        /* FIXME: Dereferencing an expr is broken so this has to be ugly */
        ptr[0] = ptr[0] | 1;
        return(ptr + 8);
    }
}

/*
 * Frees the given address from the heap.
 */
free(addr) {
    /* FIXME: Dereferencing an expr is broken so this has to be ugly */
    auto ptr;
    ptr = addr - 8;
    ptr[0] = ptr[0] ^ 1;
}

/* Used for debugging. Prints info about the current heap allocation */
printHeapMeta() {
    extrn heapBegin;
    if (heapBegin == 0) {
        putstr("Heap hasn't been initialized*n");
        return;
    }

    auto ptr, index 0;
    ptr = heapBegin;

    while (ptr[0] != 0) {
        putstr("Heap chunk ");
        putnum(index);
        putstr(" has size ");
        putnum(ptr[0] >> 1);
        putstr(" and occupied=");
        putnum(ptr[0] & 1);
        putstr(" at address ");
        putnum(ptr);
        putstr("*n");

        ptr =+ (ptr[0] >> 1) + 8;
        index =+ 1;
    }
}