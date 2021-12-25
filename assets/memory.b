_heapBegin 0;
_heapEnd 0;

/**
 * Allocates the given number of quads (64 bit values)
 * TODO: Use the buddy allocation algorithm some day...
 */
malloc(count) {
    extrn _heapBegin, _heapEnd;
    if (_heapBegin == 0) {
        _heapBegin = syscall(12, 0);
        _heapEnd = syscall(12, _heapBegin + 077777);
        /* Slot header: (size << 1) + occupiedBit  */
        /* size=0 and occupied=0 indicate the end of the chunk list */
        *_heapBegin = 0;
    }

    auto ptr, byteCount, length;
    /* Round up to the nearest 512 byte chunk */
    byteCount = (count * 8 + 0777) & ~0777;
    ptr = _heapBegin;
    /* Iterate the ptr until we hit an unoccupied slot of the right size */
    while (1) {
        length = *ptr >> 1;
        /* Unoccupied & of the right size */
        if (length == 0 | (*ptr & 1) == 0 & length == byteCount) break;
        ptr =+ length + 8;
    }

    /* If the length is 0, we're at the end */
    if (length == 0) {
        auto ptrEnd;
        ptrEnd = ptr + byteCount + 8;

        if (ptrEnd >= _heapEnd) {
            /* Increase the heap size in increments of 32KiB */
            _heapEnd = syscall(12, (ptrEnd + 077777) & ~077777);
        }
        /* Set a new null terminator */
        *ptrEnd = 0;
        *ptr = (byteCount << 1) + 1;
        return(ptr + 8);
    } else {
        /* We found a cozy spot of unallocated space that we can reuse */
        /* It just picks the first "big enough" slot. The packing is wasteful */
        *ptr =| 1;
        return(ptr + 8);
    }
}

/**
 * Frees the given address from the heap.
 */
free(addr) {
    /* Toggle the occupied bit */
    addr[-1] =^ 1;
}

/**
 * Reallocates the given address to a new size.
 * This will _always_ copy the data.
 * @param count The number of quads to reserve
 */
realloc(addr, count) {
    auto oldCount, newAddr;
    /* >> 1 to trim the occupied bit, >> 3 to divide by 8 */
    oldCount = addr[-1] >> 4;
    newAddr = malloc(count);
    memmove(newAddr, addr, oldCount);
    free(addr);
    return(newAddr);
}

/*
 * Copies data from the vector at src to the vector at dest.
 * @param count The number of quads to copy
 */
memmove(dest, src, count) {
    while (--count >= 0) {
        dest[count] = src[count];
    }
}

/**
 * Prints info about the current heap allocation.
 * It's mainly useful for debugging.
 */
printHeapMeta() {
    extrn _heapBegin;
    if (_heapBegin == 0) {
        putstr("Heap hasn't been initialized*n");
        return;
    }

    auto ptr, index 0;
    ptr = _heapBegin;

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