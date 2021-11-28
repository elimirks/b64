main() {
    auto data1, data2, data3;

    data1 = malloc(4);
    data1[0] = 'aaaaaaaa';
    data1[1] = 'bbbbbbbb';
    data1[2] = 'cccccccc';
    data1[3] = 'dddddd*n';

    data2 = malloc(2);
    data2[0] = 'data2*n';

    putstr(data1);
    putstr(data2);

    printHeapMeta();
    free(data1);
    printHeapMeta();

    data3 = malloc(1);
    printHeapMeta();
}