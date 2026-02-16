       *===============================================================
       * STRESSCOPY.cpy - generated copybook for chunking stress tests
       *===============================================================
       * NOTE: Intentionally ~10k lines with many distinct fields.
       01  STRESS-COPYBOOK-ROOT.
           05  SCB-META.
               10  SCB-VERSION                 PIC X(08) VALUE 'V0000001'.
               10  SCB-GENERATED-UTC           PIC X(20) VALUE '2026-02-12T00:00:00Z'.
               10  SCB-ENV-TAG                 PIC X(12) VALUE 'CHUNK-TEST'.
           05  SCB-FILLER                      PIC X(10) VALUE SPACES.
           05  SCB-GRP-0001.
               10  SCB-B-000001             PIC S9(07)V99 COMP-3 VALUE +0000001.
               10  SCB-C-000002             PIC X(24) VALUE 'FIELD-000002-ALPHA'.
               10  SCB-D-000003             PIC 9(04) VALUE 21.
               10  SCB-DESC-000003          PIC X(18) VALUE 'DESC-000039'.
               10  SCB-E-000004             OCCURS 2 TIMES.
                   15  SCB-EK-000004        PIC X(10).
                   15  SCB-EV-000004        PIC 9(06) COMP.
               10  SCB-A-000005             PIC 9(09) COMP VALUE 5.
               10  SCB-B-000006             PIC S9(07)V99 COMP-3 VALUE +0000006.
               10  SCB-C-000007             PIC X(24) VALUE 'FIELD-000007-ALPHA'.
               10  SCB-D-000008             PIC 9(04) VALUE 56.
               10  SCB-DESC-000008          PIC X(18) VALUE 'DESC-000104'.
               10  SCB-E-000009             OCCURS 3 TIMES.
                   15  SCB-EK-000009        PIC X(10).
                   15  SCB-EV-000009        PIC 9(06) COMP.
           05  SCB-GRP-0002.
               10  SCB-A-000010             PIC 9(09) COMP VALUE 10.
               10  SCB-B-000011             PIC S9(07)V99 COMP-3 VALUE +0000011.
               10  SCB-C-000012             PIC X(24) VALUE 'FIELD-000012-ALPHA'.
               10  SCB-D-000013             PIC 9(04) VALUE 91.
               10  SCB-DESC-000013          PIC X(18) VALUE 'DESC-000169'.
               10  SCB-E-000014             OCCURS 4 TIMES.
                   15  SCB-EK-000014        PIC X(10).
                   15  SCB-EV-000014        PIC 9(06) COMP.
               10  SCB-A-000015             PIC 9(09) COMP VALUE 15.
               10  SCB-B-000016             PIC S9(07)V99 COMP-3 VALUE +0000016.
               10  SCB-C-000017             PIC X(24) VALUE 'FIELD-000017-ALPHA'.
               10  SCB-D-000018             PIC 9(04) VALUE 126.
               10  SCB-DESC-000018          PIC X(18) VALUE 'DESC-000234'.
               10  SCB-E-000019             OCCURS 5 TIMES.
                   15  SCB-EK-000019        PIC X(10).
                   15  SCB-EV-000019        PIC 9(06) COMP.
           05  SCB-GRP-0003.
               10  SCB-A-000020             PIC 9(09) COMP VALUE 20.
               10  SCB-B-000021             PIC S9(07)V99 COMP-3 VALUE +0000021.
               10  SCB-C-000022             PIC X(24) VALUE 'FIELD-000022-ALPHA'.
               10  SCB-D-000023             PIC 9(04) VALUE 161.
               10  SCB-DESC-000023          PIC X(18) VALUE 'DESC-000299'.
               10  SCB-E-000024             OCCURS 2 TIMES.
                   15  SCB-EK-000024        PIC X(10).
                   15  SCB-EV-000024        PIC 9(06) COMP.
               10  SCB-A-000025             PIC 9(09) COMP VALUE 25.
               10  SCB-B-000026             PIC S9(07)V99 COMP-3 VALUE +0000026.
               10  SCB-C-000027             PIC X(24) VALUE 'FIELD-000027-ALPHA'.
               10  SCB-D-000028             PIC 9(04) VALUE 196.
               10  SCB-DESC-000028          PIC X(18) VALUE 'DESC-000364'.
               10  SCB-E-000029             OCCURS 3 TIMES.
                   15  SCB-EK-000029        PIC X(10).
                   15  SCB-EV-000029        PIC 9(06) COMP.
               10  SCB-A-000030             PIC 9(09) COMP VALUE 30.
           05  SCB-GRP-0004.
               10  SCB-B-000031             PIC S9(07)V99 COMP-3 VALUE +0000031.
               10  SCB-C-000032             PIC X(24) VALUE 'FIELD-000032-ALPHA'.
               10  SCB-D-000033             PIC 9(04) VALUE 231.
               10  SCB-DESC-000033          PIC X(18) VALUE 'DESC-000429'.
               10  SCB-E-000034             OCCURS 4 TIMES.
                   15  SCB-EK-000034        PIC X(10).
                   15  SCB-EV-000034        PIC 9(06) COMP.
               10  SCB-A-000035             PIC 9(09) COMP VALUE 35.
               10  SCB-B-000036             PIC S9(07)V99 COMP-3 VALUE +0000036.
               10  SCB-C-000037             PIC X(24) VALUE 'FIELD-000037-ALPHA'.
               10  SCB-D-000038             PIC 9(04) VALUE 266.
               10  SCB-DESC-000038          PIC X(18) VALUE 'DESC-000494'.
               10  SCB-E-000039             OCCURS 5 TIMES.
                   15  SCB-EK-000039        PIC X(10).
                   15  SCB-EV-000039        PIC 9(06) COMP.
               10  SCB-A-000040             PIC 9(09) COMP VALUE 40.
               10  SCB-B-000041             PIC S9(07)V99 COMP-3 VALUE +0000041.
               10  SCB-C-000042             PIC X(24) VALUE 'FIELD-000042-ALPHA'.
           05  SCB-GRP-0005.
               10  SCB-D-000043             PIC 9(04) VALUE 301.
               10  SCB-DESC-000043          PIC X(18) VALUE 'DESC-000559'.
               10  SCB-E-000044             OCCURS 2 TIMES.
                   15  SCB-EK-000044        PIC X(10).
                   15  SCB-EV-000044        PIC 9(06) COMP.
               10  SCB-A-000045             PIC 9(09) COMP VALUE 45.
               10  SCB-B-000046             PIC S9(07)V99 COMP-3 VALUE +0000046.
               10  SCB-C-000047             PIC X(24) VALUE 'FIELD-000047-ALPHA'.
               10  SCB-D-000048             PIC 9(04) VALUE 336.
               10  SCB-DESC-000048          PIC X(18) VALUE 'DESC-000624'.
               10  SCB-E-000049             OCCURS 3 TIMES.
                   15  SCB-EK-000049        PIC X(10).
                   15  SCB-EV-000049        PIC 9(06) COMP.
               10  SCB-A-000050             PIC 9(09) COMP VALUE 50.
               10  SCB-B-000051             PIC S9(07)V99 COMP-3 VALUE +0000051.
               10  SCB-C-000052             PIC X(24) VALUE 'FIELD-000052-ALPHA'.
               10  SCB-D-000053             PIC 9(04) VALUE 371.
               10  SCB-DESC-000053          PIC X(18) VALUE 'DESC-000689'.
               10  SCB-E-000054             OCCURS 4 TIMES.
                   15  SCB-EK-000054        PIC X(10).
                   15  SCB-EV-000054        PIC 9(06) COMP.
               10  SCB-A-000055             PIC 9(09) COMP VALUE 55.
           05  SCB-GRP-0006.
               10  SCB-B-000056             PIC S9(07)V99 COMP-3 VALUE +0000056.
               10  SCB-C-000057             PIC X(24) VALUE 'FIELD-000057-ALPHA'.
               10  SCB-D-000058             PIC 9(04) VALUE 406.
               10  SCB-DESC-000058          PIC X(18) VALUE 'DESC-000754'.
               10  SCB-E-000059             OCCURS 5 TIMES.
                   15  SCB-EK-000059        PIC X(10).
                   15  SCB-EV-000059        PIC 9(06) COMP.
               10  SCB-A-000060             PIC 9(09) COMP VALUE 60.
               10  SCB-B-000061             PIC S9(07)V99 COMP-3 VALUE +0000061.
               10  SCB-C-000062             PIC X(24) VALUE 'FIELD-000062-ALPHA'.
               10  SCB-D-000063             PIC 9(04) VALUE 441.
               10  SCB-DESC-000063          PIC X(18) VALUE 'DESC-000819'.
               10  SCB-E-000064             OCCURS 2 TIMES.
                   15  SCB-EK-000064        PIC X(10).
                   15  SCB-EV-000064        PIC 9(06) COMP.
               10  SCB-A-000065             PIC 9(09) COMP VALUE 65.
               10  SCB-B-000066             PIC S9(07)V99 COMP-3 VALUE +0000066.
               10  SCB-C-000067             PIC X(24) VALUE 'FIELD-000067-ALPHA'.
               10  SCB-D-000068             PIC 9(04) VALUE 476.
               10  SCB-DESC-000068          PIC X(18) VALUE 'DESC-000884'.
               10  SCB-E-000069             OCCURS 3 TIMES.
                   15  SCB-EK-000069        PIC X(10).
                   15  SCB-EV-000069        PIC 9(06) COMP.
           05  SCB-ALT-0001 REDEFINES SCB-GRP-0006.
               10  SCB-ALT-FLAG-0001      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0001   PIC X(64).
           05  SCB-GRP-0007.
               10  SCB-A-000070             PIC 9(09) COMP VALUE 70.
               10  SCB-B-000071             PIC S9(07)V99 COMP-3 VALUE +0000071.
               10  SCB-C-000072             PIC X(24) VALUE 'FIELD-000072-ALPHA'.
               10  SCB-D-000073             PIC 9(04) VALUE 511.
               10  SCB-DESC-000073          PIC X(18) VALUE 'DESC-000949'.
               10  SCB-E-000074             OCCURS 4 TIMES.
                   15  SCB-EK-000074        PIC X(10).
                   15  SCB-EV-000074        PIC 9(06) COMP.
               10  SCB-A-000075             PIC 9(09) COMP VALUE 75.
               10  SCB-B-000076             PIC S9(07)V99 COMP-3 VALUE +0000076.
               10  SCB-C-000077             PIC X(24) VALUE 'FIELD-000077-ALPHA'.
           05  SCB-GRP-0008.
               10  SCB-D-000078             PIC 9(04) VALUE 546.
               10  SCB-DESC-000078          PIC X(18) VALUE 'DESC-001014'.
               10  SCB-E-000079             OCCURS 5 TIMES.
                   15  SCB-EK-000079        PIC X(10).
                   15  SCB-EV-000079        PIC 9(06) COMP.
               10  SCB-A-000080             PIC 9(09) COMP VALUE 80.
               10  SCB-B-000081             PIC S9(07)V99 COMP-3 VALUE +0000081.
               10  SCB-C-000082             PIC X(24) VALUE 'FIELD-000082-ALPHA'.
               10  SCB-D-000083             PIC 9(04) VALUE 581.
               10  SCB-DESC-000083          PIC X(18) VALUE 'DESC-001079'.
               10  SCB-E-000084             OCCURS 2 TIMES.
                   15  SCB-EK-000084        PIC X(10).
                   15  SCB-EV-000084        PIC 9(06) COMP.
               10  SCB-A-000085             PIC 9(09) COMP VALUE 85.
               10  SCB-B-000086             PIC S9(07)V99 COMP-3 VALUE +0000086.
           05  SCB-GRP-0009.
               10  SCB-C-000087             PIC X(24) VALUE 'FIELD-000087-ALPHA'.
               10  SCB-D-000088             PIC 9(04) VALUE 616.
               10  SCB-DESC-000088          PIC X(18) VALUE 'DESC-001144'.
               10  SCB-E-000089             OCCURS 3 TIMES.
                   15  SCB-EK-000089        PIC X(10).
                   15  SCB-EV-000089        PIC 9(06) COMP.
               10  SCB-A-000090             PIC 9(09) COMP VALUE 90.
               10  SCB-B-000091             PIC S9(07)V99 COMP-3 VALUE +0000091.
               10  SCB-C-000092             PIC X(24) VALUE 'FIELD-000092-ALPHA'.
               10  SCB-D-000093             PIC 9(04) VALUE 651.
               10  SCB-DESC-000093          PIC X(18) VALUE 'DESC-001209'.
               10  SCB-E-000094             OCCURS 4 TIMES.
                   15  SCB-EK-000094        PIC X(10).
                   15  SCB-EV-000094        PIC 9(06) COMP.
               10  SCB-A-000095             PIC 9(09) COMP VALUE 95.
               10  SCB-B-000096             PIC S9(07)V99 COMP-3 VALUE +0000096.
           05  SCB-GRP-0010.
               10  SCB-C-000097             PIC X(24) VALUE 'FIELD-000097-ALPHA'.
               10  SCB-D-000098             PIC 9(04) VALUE 686.
               10  SCB-DESC-000098          PIC X(18) VALUE 'DESC-001274'.
               10  SCB-E-000099             OCCURS 5 TIMES.
                   15  SCB-EK-000099        PIC X(10).
                   15  SCB-EV-000099        PIC 9(06) COMP.
               10  SCB-A-000100             PIC 9(09) COMP VALUE 100.
               10  SCB-B-000101             PIC S9(07)V99 COMP-3 VALUE +0000101.
               10  SCB-C-000102             PIC X(24) VALUE 'FIELD-000102-ALPHA'.
               10  SCB-D-000103             PIC 9(04) VALUE 721.
               10  SCB-DESC-000103          PIC X(18) VALUE 'DESC-001339'.
               10  SCB-E-000104             OCCURS 2 TIMES.
                   15  SCB-EK-000104        PIC X(10).
                   15  SCB-EV-000104        PIC 9(06) COMP.
               10  SCB-A-000105             PIC 9(09) COMP VALUE 105.
               10  SCB-B-000106             PIC S9(07)V99 COMP-3 VALUE +0000106.
               10  SCB-C-000107             PIC X(24) VALUE 'FIELD-000107-ALPHA'.
           05  SCB-GRP-0011.
               10  SCB-D-000108             PIC 9(04) VALUE 756.
               10  SCB-DESC-000108          PIC X(18) VALUE 'DESC-001404'.
               10  SCB-E-000109             OCCURS 3 TIMES.
                   15  SCB-EK-000109        PIC X(10).
                   15  SCB-EV-000109        PIC 9(06) COMP.
               10  SCB-A-000110             PIC 9(09) COMP VALUE 110.
               10  SCB-B-000111             PIC S9(07)V99 COMP-3 VALUE +0000111.
               10  SCB-C-000112             PIC X(24) VALUE 'FIELD-000112-ALPHA'.
               10  SCB-D-000113             PIC 9(04) VALUE 791.
               10  SCB-DESC-000113          PIC X(18) VALUE 'DESC-001469'.
               10  SCB-E-000114             OCCURS 4 TIMES.
                   15  SCB-EK-000114        PIC X(10).
                   15  SCB-EV-000114        PIC 9(06) COMP.
               10  SCB-A-000115             PIC 9(09) COMP VALUE 115.
               10  SCB-B-000116             PIC S9(07)V99 COMP-3 VALUE +0000116.
               10  SCB-C-000117             PIC X(24) VALUE 'FIELD-000117-ALPHA'.
               10  SCB-D-000118             PIC 9(04) VALUE 826.
               10  SCB-DESC-000118          PIC X(18) VALUE 'DESC-001534'.
               10  SCB-E-000119             OCCURS 5 TIMES.
                   15  SCB-EK-000119        PIC X(10).
                   15  SCB-EV-000119        PIC 9(06) COMP.
           05  SCB-GRP-0012.
               10  SCB-A-000120             PIC 9(09) COMP VALUE 120.
               10  SCB-B-000121             PIC S9(07)V99 COMP-3 VALUE +0000121.
               10  SCB-C-000122             PIC X(24) VALUE 'FIELD-000122-ALPHA'.
               10  SCB-D-000123             PIC 9(04) VALUE 861.
               10  SCB-DESC-000123          PIC X(18) VALUE 'DESC-001599'.
               10  SCB-E-000124             OCCURS 2 TIMES.
                   15  SCB-EK-000124        PIC X(10).
                   15  SCB-EV-000124        PIC 9(06) COMP.
               10  SCB-A-000125             PIC 9(09) COMP VALUE 125.
               10  SCB-B-000126             PIC S9(07)V99 COMP-3 VALUE +0000126.
               10  SCB-C-000127             PIC X(24) VALUE 'FIELD-000127-ALPHA'.
               10  SCB-D-000128             PIC 9(04) VALUE 896.
               10  SCB-DESC-000128          PIC X(18) VALUE 'DESC-001664'.
               10  SCB-E-000129             OCCURS 3 TIMES.
                   15  SCB-EK-000129        PIC X(10).
                   15  SCB-EV-000129        PIC 9(06) COMP.
               10  SCB-A-000130             PIC 9(09) COMP VALUE 130.
               10  SCB-B-000131             PIC S9(07)V99 COMP-3 VALUE +0000131.
               10  SCB-C-000132             PIC X(24) VALUE 'FIELD-000132-ALPHA'.
           05  SCB-ALT-0002 REDEFINES SCB-GRP-0012.
               10  SCB-ALT-FLAG-0002      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0002   PIC X(64).
           05  SCB-GRP-0013.
               10  SCB-D-000133             PIC 9(04) VALUE 931.
               10  SCB-DESC-000133          PIC X(18) VALUE 'DESC-001729'.
               10  SCB-E-000134             OCCURS 4 TIMES.
                   15  SCB-EK-000134        PIC X(10).
                   15  SCB-EV-000134        PIC 9(06) COMP.
               10  SCB-A-000135             PIC 9(09) COMP VALUE 135.
               10  SCB-B-000136             PIC S9(07)V99 COMP-3 VALUE +0000136.
               10  SCB-C-000137             PIC X(24) VALUE 'FIELD-000137-ALPHA'.
               10  SCB-D-000138             PIC 9(04) VALUE 966.
               10  SCB-DESC-000138          PIC X(18) VALUE 'DESC-001794'.
               10  SCB-E-000139             OCCURS 5 TIMES.
                   15  SCB-EK-000139        PIC X(10).
                   15  SCB-EV-000139        PIC 9(06) COMP.
               10  SCB-A-000140             PIC 9(09) COMP VALUE 140.
               10  SCB-B-000141             PIC S9(07)V99 COMP-3 VALUE +0000141.
               10  SCB-C-000142             PIC X(24) VALUE 'FIELD-000142-ALPHA'.
               10  SCB-D-000143             PIC 9(04) VALUE 1001.
               10  SCB-DESC-000143          PIC X(18) VALUE 'DESC-001859'.
               10  SCB-E-000144             OCCURS 2 TIMES.
                   15  SCB-EK-000144        PIC X(10).
                   15  SCB-EV-000144        PIC 9(06) COMP.
               10  SCB-A-000145             PIC 9(09) COMP VALUE 145.
               10  SCB-B-000146             PIC S9(07)V99 COMP-3 VALUE +0000146.
           05  SCB-GRP-0014.
               10  SCB-C-000147             PIC X(24) VALUE 'FIELD-000147-ALPHA'.
               10  SCB-D-000148             PIC 9(04) VALUE 1036.
               10  SCB-DESC-000148          PIC X(18) VALUE 'DESC-001924'.
               10  SCB-E-000149             OCCURS 3 TIMES.
                   15  SCB-EK-000149        PIC X(10).
                   15  SCB-EV-000149        PIC 9(06) COMP.
               10  SCB-A-000150             PIC 9(09) COMP VALUE 150.
               10  SCB-B-000151             PIC S9(07)V99 COMP-3 VALUE +0000151.
               10  SCB-C-000152             PIC X(24) VALUE 'FIELD-000152-ALPHA'.
               10  SCB-D-000153             PIC 9(04) VALUE 1071.
               10  SCB-DESC-000153          PIC X(18) VALUE 'DESC-001989'.
               10  SCB-E-000154             OCCURS 4 TIMES.
                   15  SCB-EK-000154        PIC X(10).
                   15  SCB-EV-000154        PIC 9(06) COMP.
           05  SCB-GRP-0015.
               10  SCB-A-000155             PIC 9(09) COMP VALUE 155.
               10  SCB-B-000156             PIC S9(07)V99 COMP-3 VALUE +0000156.
               10  SCB-C-000157             PIC X(24) VALUE 'FIELD-000157-ALPHA'.
               10  SCB-D-000158             PIC 9(04) VALUE 1106.
               10  SCB-DESC-000158          PIC X(18) VALUE 'DESC-002054'.
               10  SCB-E-000159             OCCURS 5 TIMES.
                   15  SCB-EK-000159        PIC X(10).
                   15  SCB-EV-000159        PIC 9(06) COMP.
               10  SCB-A-000160             PIC 9(09) COMP VALUE 160.
               10  SCB-B-000161             PIC S9(07)V99 COMP-3 VALUE +0000161.
               10  SCB-C-000162             PIC X(24) VALUE 'FIELD-000162-ALPHA'.
               10  SCB-D-000163             PIC 9(04) VALUE 1141.
               10  SCB-DESC-000163          PIC X(18) VALUE 'DESC-002119'.
           05  SCB-GRP-0016.
               10  SCB-E-000164             OCCURS 2 TIMES.
                   15  SCB-EK-000164        PIC X(10).
                   15  SCB-EV-000164        PIC 9(06) COMP.
               10  SCB-A-000165             PIC 9(09) COMP VALUE 165.
               10  SCB-B-000166             PIC S9(07)V99 COMP-3 VALUE +0000166.
               10  SCB-C-000167             PIC X(24) VALUE 'FIELD-000167-ALPHA'.
               10  SCB-D-000168             PIC 9(04) VALUE 1176.
               10  SCB-DESC-000168          PIC X(18) VALUE 'DESC-002184'.
               10  SCB-E-000169             OCCURS 3 TIMES.
                   15  SCB-EK-000169        PIC X(10).
                   15  SCB-EV-000169        PIC 9(06) COMP.
               10  SCB-A-000170             PIC 9(09) COMP VALUE 170.
               10  SCB-B-000171             PIC S9(07)V99 COMP-3 VALUE +0000171.
               10  SCB-C-000172             PIC X(24) VALUE 'FIELD-000172-ALPHA'.
               10  SCB-D-000173             PIC 9(04) VALUE 1211.
               10  SCB-DESC-000173          PIC X(18) VALUE 'DESC-002249'.
           05  SCB-GRP-0017.
               10  SCB-E-000174             OCCURS 4 TIMES.
                   15  SCB-EK-000174        PIC X(10).
                   15  SCB-EV-000174        PIC 9(06) COMP.
               10  SCB-A-000175             PIC 9(09) COMP VALUE 175.
               10  SCB-B-000176             PIC S9(07)V99 COMP-3 VALUE +0000176.
               10  SCB-C-000177             PIC X(24) VALUE 'FIELD-000177-ALPHA'.
               10  SCB-D-000178             PIC 9(04) VALUE 1246.
               10  SCB-DESC-000178          PIC X(18) VALUE 'DESC-002314'.
               10  SCB-E-000179             OCCURS 5 TIMES.
                   15  SCB-EK-000179        PIC X(10).
                   15  SCB-EV-000179        PIC 9(06) COMP.
               10  SCB-A-000180             PIC 9(09) COMP VALUE 180.
               10  SCB-B-000181             PIC S9(07)V99 COMP-3 VALUE +0000181.
               10  SCB-C-000182             PIC X(24) VALUE 'FIELD-000182-ALPHA'.
               10  SCB-D-000183             PIC 9(04) VALUE 1281.
               10  SCB-DESC-000183          PIC X(18) VALUE 'DESC-002379'.
               10  SCB-E-000184             OCCURS 2 TIMES.
                   15  SCB-EK-000184        PIC X(10).
                   15  SCB-EV-000184        PIC 9(06) COMP.
           05  SCB-GRP-0018.
               10  SCB-A-000185             PIC 9(09) COMP VALUE 185.
               10  SCB-B-000186             PIC S9(07)V99 COMP-3 VALUE +0000186.
               10  SCB-C-000187             PIC X(24) VALUE 'FIELD-000187-ALPHA'.
               10  SCB-D-000188             PIC 9(04) VALUE 1316.
               10  SCB-DESC-000188          PIC X(18) VALUE 'DESC-002444'.
               10  SCB-E-000189             OCCURS 3 TIMES.
                   15  SCB-EK-000189        PIC X(10).
                   15  SCB-EV-000189        PIC 9(06) COMP.
               10  SCB-A-000190             PIC 9(09) COMP VALUE 190.
               10  SCB-B-000191             PIC S9(07)V99 COMP-3 VALUE +0000191.
               10  SCB-C-000192             PIC X(24) VALUE 'FIELD-000192-ALPHA'.
               10  SCB-D-000193             PIC 9(04) VALUE 1351.
               10  SCB-DESC-000193          PIC X(18) VALUE 'DESC-002509'.
               10  SCB-E-000194             OCCURS 4 TIMES.
                   15  SCB-EK-000194        PIC X(10).
                   15  SCB-EV-000194        PIC 9(06) COMP.
               10  SCB-A-000195             PIC 9(09) COMP VALUE 195.
               10  SCB-B-000196             PIC S9(07)V99 COMP-3 VALUE +0000196.
           05  SCB-ALT-0003 REDEFINES SCB-GRP-0018.
               10  SCB-ALT-FLAG-0003      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0003   PIC X(64).
           05  SCB-GRP-0019.
               10  SCB-C-000197             PIC X(24) VALUE 'FIELD-000197-ALPHA'.
               10  SCB-D-000198             PIC 9(04) VALUE 1386.
               10  SCB-DESC-000198          PIC X(18) VALUE 'DESC-002574'.
               10  SCB-E-000199             OCCURS 5 TIMES.
                   15  SCB-EK-000199        PIC X(10).
                   15  SCB-EV-000199        PIC 9(06) COMP.
               10  SCB-A-000200             PIC 9(09) COMP VALUE 200.
               10  SCB-B-000201             PIC S9(07)V99 COMP-3 VALUE +0000201.
               10  SCB-C-000202             PIC X(24) VALUE 'FIELD-000202-ALPHA'.
               10  SCB-D-000203             PIC 9(04) VALUE 1421.
               10  SCB-DESC-000203          PIC X(18) VALUE 'DESC-002639'.
               10  SCB-E-000204             OCCURS 2 TIMES.
                   15  SCB-EK-000204        PIC X(10).
                   15  SCB-EV-000204        PIC 9(06) COMP.
               10  SCB-A-000205             PIC 9(09) COMP VALUE 205.
               10  SCB-B-000206             PIC S9(07)V99 COMP-3 VALUE +0000206.
               10  SCB-C-000207             PIC X(24) VALUE 'FIELD-000207-ALPHA'.
               10  SCB-D-000208             PIC 9(04) VALUE 1456.
               10  SCB-DESC-000208          PIC X(18) VALUE 'DESC-002704'.
               10  SCB-E-000209             OCCURS 3 TIMES.
                   15  SCB-EK-000209        PIC X(10).
                   15  SCB-EV-000209        PIC 9(06) COMP.
           05  SCB-GRP-0020.
               10  SCB-A-000210             PIC 9(09) COMP VALUE 210.
               10  SCB-B-000211             PIC S9(07)V99 COMP-3 VALUE +0000211.
               10  SCB-C-000212             PIC X(24) VALUE 'FIELD-000212-ALPHA'.
               10  SCB-D-000213             PIC 9(04) VALUE 1491.
               10  SCB-DESC-000213          PIC X(18) VALUE 'DESC-002769'.
               10  SCB-E-000214             OCCURS 4 TIMES.
                   15  SCB-EK-000214        PIC X(10).
                   15  SCB-EV-000214        PIC 9(06) COMP.
               10  SCB-A-000215             PIC 9(09) COMP VALUE 215.
               10  SCB-B-000216             PIC S9(07)V99 COMP-3 VALUE +0000216.
               10  SCB-C-000217             PIC X(24) VALUE 'FIELD-000217-ALPHA'.
               10  SCB-D-000218             PIC 9(04) VALUE 1526.
               10  SCB-DESC-000218          PIC X(18) VALUE 'DESC-002834'.
               10  SCB-E-000219             OCCURS 5 TIMES.
                   15  SCB-EK-000219        PIC X(10).
                   15  SCB-EV-000219        PIC 9(06) COMP.
               10  SCB-A-000220             PIC 9(09) COMP VALUE 220.
               10  SCB-B-000221             PIC S9(07)V99 COMP-3 VALUE +0000221.
               10  SCB-C-000222             PIC X(24) VALUE 'FIELD-000222-ALPHA'.
               10  SCB-D-000223             PIC 9(04) VALUE 1561.
               10  SCB-DESC-000223          PIC X(18) VALUE 'DESC-002899'.
           05  SCB-GRP-0021.
               10  SCB-E-000224             OCCURS 2 TIMES.
                   15  SCB-EK-000224        PIC X(10).
                   15  SCB-EV-000224        PIC 9(06) COMP.
               10  SCB-A-000225             PIC 9(09) COMP VALUE 225.
               10  SCB-B-000226             PIC S9(07)V99 COMP-3 VALUE +0000226.
               10  SCB-C-000227             PIC X(24) VALUE 'FIELD-000227-ALPHA'.
               10  SCB-D-000228             PIC 9(04) VALUE 1596.
               10  SCB-DESC-000228          PIC X(18) VALUE 'DESC-002964'.
               10  SCB-E-000229             OCCURS 3 TIMES.
                   15  SCB-EK-000229        PIC X(10).
                   15  SCB-EV-000229        PIC 9(06) COMP.
               10  SCB-A-000230             PIC 9(09) COMP VALUE 230.
               10  SCB-B-000231             PIC S9(07)V99 COMP-3 VALUE +0000231.
           05  SCB-GRP-0022.
               10  SCB-C-000232             PIC X(24) VALUE 'FIELD-000232-ALPHA'.
               10  SCB-D-000233             PIC 9(04) VALUE 1631.
               10  SCB-DESC-000233          PIC X(18) VALUE 'DESC-003029'.
               10  SCB-E-000234             OCCURS 4 TIMES.
                   15  SCB-EK-000234        PIC X(10).
                   15  SCB-EV-000234        PIC 9(06) COMP.
               10  SCB-A-000235             PIC 9(09) COMP VALUE 235.
               10  SCB-B-000236             PIC S9(07)V99 COMP-3 VALUE +0000236.
               10  SCB-C-000237             PIC X(24) VALUE 'FIELD-000237-ALPHA'.
               10  SCB-D-000238             PIC 9(04) VALUE 1666.
               10  SCB-DESC-000238          PIC X(18) VALUE 'DESC-003094'.
               10  SCB-E-000239             OCCURS 5 TIMES.
                   15  SCB-EK-000239        PIC X(10).
                   15  SCB-EV-000239        PIC 9(06) COMP.
               10  SCB-A-000240             PIC 9(09) COMP VALUE 240.
           05  SCB-GRP-0023.
               10  SCB-B-000241             PIC S9(07)V99 COMP-3 VALUE +0000241.
               10  SCB-C-000242             PIC X(24) VALUE 'FIELD-000242-ALPHA'.
               10  SCB-D-000243             PIC 9(04) VALUE 1701.
               10  SCB-DESC-000243          PIC X(18) VALUE 'DESC-003159'.
               10  SCB-E-000244             OCCURS 2 TIMES.
                   15  SCB-EK-000244        PIC X(10).
                   15  SCB-EV-000244        PIC 9(06) COMP.
               10  SCB-A-000245             PIC 9(09) COMP VALUE 245.
               10  SCB-B-000246             PIC S9(07)V99 COMP-3 VALUE +0000246.
               10  SCB-C-000247             PIC X(24) VALUE 'FIELD-000247-ALPHA'.
               10  SCB-D-000248             PIC 9(04) VALUE 1736.
               10  SCB-DESC-000248          PIC X(18) VALUE 'DESC-003224'.
               10  SCB-E-000249             OCCURS 3 TIMES.
                   15  SCB-EK-000249        PIC X(10).
                   15  SCB-EV-000249        PIC 9(06) COMP.
               10  SCB-A-000250             PIC 9(09) COMP VALUE 250.
           05  SCB-GRP-0024.
               10  SCB-B-000251             PIC S9(07)V99 COMP-3 VALUE +0000251.
               10  SCB-C-000252             PIC X(24) VALUE 'FIELD-000252-ALPHA'.
               10  SCB-D-000253             PIC 9(04) VALUE 1771.
               10  SCB-DESC-000253          PIC X(18) VALUE 'DESC-003289'.
               10  SCB-E-000254             OCCURS 4 TIMES.
                   15  SCB-EK-000254        PIC X(10).
                   15  SCB-EV-000254        PIC 9(06) COMP.
               10  SCB-A-000255             PIC 9(09) COMP VALUE 255.
               10  SCB-B-000256             PIC S9(07)V99 COMP-3 VALUE +0000256.
               10  SCB-C-000257             PIC X(24) VALUE 'FIELD-000257-ALPHA'.
               10  SCB-D-000258             PIC 9(04) VALUE 1806.
               10  SCB-DESC-000258          PIC X(18) VALUE 'DESC-003354'.
               10  SCB-E-000259             OCCURS 5 TIMES.
                   15  SCB-EK-000259        PIC X(10).
                   15  SCB-EV-000259        PIC 9(06) COMP.
               10  SCB-A-000260             PIC 9(09) COMP VALUE 260.
               10  SCB-B-000261             PIC S9(07)V99 COMP-3 VALUE +0000261.
           05  SCB-ALT-0004 REDEFINES SCB-GRP-0024.
               10  SCB-ALT-FLAG-0004      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0004   PIC X(64).
           05  SCB-GRP-0025.
               10  SCB-C-000262             PIC X(24) VALUE 'FIELD-000262-ALPHA'.
               10  SCB-D-000263             PIC 9(04) VALUE 1841.
               10  SCB-DESC-000263          PIC X(18) VALUE 'DESC-003419'.
               10  SCB-E-000264             OCCURS 2 TIMES.
                   15  SCB-EK-000264        PIC X(10).
                   15  SCB-EV-000264        PIC 9(06) COMP.
               10  SCB-A-000265             PIC 9(09) COMP VALUE 265.
               10  SCB-B-000266             PIC S9(07)V99 COMP-3 VALUE +0000266.
               10  SCB-C-000267             PIC X(24) VALUE 'FIELD-000267-ALPHA'.
               10  SCB-D-000268             PIC 9(04) VALUE 1876.
               10  SCB-DESC-000268          PIC X(18) VALUE 'DESC-003484'.
               10  SCB-E-000269             OCCURS 3 TIMES.
                   15  SCB-EK-000269        PIC X(10).
                   15  SCB-EV-000269        PIC 9(06) COMP.
               10  SCB-A-000270             PIC 9(09) COMP VALUE 270.
               10  SCB-B-000271             PIC S9(07)V99 COMP-3 VALUE +0000271.
               10  SCB-C-000272             PIC X(24) VALUE 'FIELD-000272-ALPHA'.
               10  SCB-D-000273             PIC 9(04) VALUE 1911.
               10  SCB-DESC-000273          PIC X(18) VALUE 'DESC-003549'.
           05  SCB-GRP-0026.
               10  SCB-E-000274             OCCURS 4 TIMES.
                   15  SCB-EK-000274        PIC X(10).
                   15  SCB-EV-000274        PIC 9(06) COMP.
               10  SCB-A-000275             PIC 9(09) COMP VALUE 275.
               10  SCB-B-000276             PIC S9(07)V99 COMP-3 VALUE +0000276.
               10  SCB-C-000277             PIC X(24) VALUE 'FIELD-000277-ALPHA'.
               10  SCB-D-000278             PIC 9(04) VALUE 1946.
               10  SCB-DESC-000278          PIC X(18) VALUE 'DESC-003614'.
               10  SCB-E-000279             OCCURS 5 TIMES.
                   15  SCB-EK-000279        PIC X(10).
                   15  SCB-EV-000279        PIC 9(06) COMP.
               10  SCB-A-000280             PIC 9(09) COMP VALUE 280.
               10  SCB-B-000281             PIC S9(07)V99 COMP-3 VALUE +0000281.
               10  SCB-C-000282             PIC X(24) VALUE 'FIELD-000282-ALPHA'.
               10  SCB-D-000283             PIC 9(04) VALUE 1981.
               10  SCB-DESC-000283          PIC X(18) VALUE 'DESC-003679'.
               10  SCB-E-000284             OCCURS 2 TIMES.
                   15  SCB-EK-000284        PIC X(10).
                   15  SCB-EV-000284        PIC 9(06) COMP.
               10  SCB-A-000285             PIC 9(09) COMP VALUE 285.
               10  SCB-B-000286             PIC S9(07)V99 COMP-3 VALUE +0000286.
           05  SCB-GRP-0027.
               10  SCB-C-000287             PIC X(24) VALUE 'FIELD-000287-ALPHA'.
               10  SCB-D-000288             PIC 9(04) VALUE 2016.
               10  SCB-DESC-000288          PIC X(18) VALUE 'DESC-003744'.
               10  SCB-E-000289             OCCURS 3 TIMES.
                   15  SCB-EK-000289        PIC X(10).
                   15  SCB-EV-000289        PIC 9(06) COMP.
               10  SCB-A-000290             PIC 9(09) COMP VALUE 290.
               10  SCB-B-000291             PIC S9(07)V99 COMP-3 VALUE +0000291.
               10  SCB-C-000292             PIC X(24) VALUE 'FIELD-000292-ALPHA'.
               10  SCB-D-000293             PIC 9(04) VALUE 2051.
               10  SCB-DESC-000293          PIC X(18) VALUE 'DESC-003809'.
               10  SCB-E-000294             OCCURS 4 TIMES.
                   15  SCB-EK-000294        PIC X(10).
                   15  SCB-EV-000294        PIC 9(06) COMP.
               10  SCB-A-000295             PIC 9(09) COMP VALUE 295.
               10  SCB-B-000296             PIC S9(07)V99 COMP-3 VALUE +0000296.
               10  SCB-C-000297             PIC X(24) VALUE 'FIELD-000297-ALPHA'.
               10  SCB-D-000298             PIC 9(04) VALUE 2086.
               10  SCB-DESC-000298          PIC X(18) VALUE 'DESC-003874'.
               10  SCB-E-000299             OCCURS 5 TIMES.
                   15  SCB-EK-000299        PIC X(10).
                   15  SCB-EV-000299        PIC 9(06) COMP.
               10  SCB-A-000300             PIC 9(09) COMP VALUE 300.
           05  SCB-GRP-0028.
               10  SCB-B-000301             PIC S9(07)V99 COMP-3 VALUE +0000301.
               10  SCB-C-000302             PIC X(24) VALUE 'FIELD-000302-ALPHA'.
               10  SCB-D-000303             PIC 9(04) VALUE 2121.
               10  SCB-DESC-000303          PIC X(18) VALUE 'DESC-003939'.
               10  SCB-E-000304             OCCURS 2 TIMES.
                   15  SCB-EK-000304        PIC X(10).
                   15  SCB-EV-000304        PIC 9(06) COMP.
               10  SCB-A-000305             PIC 9(09) COMP VALUE 305.
               10  SCB-B-000306             PIC S9(07)V99 COMP-3 VALUE +0000306.
               10  SCB-C-000307             PIC X(24) VALUE 'FIELD-000307-ALPHA'.
               10  SCB-D-000308             PIC 9(04) VALUE 2156.
               10  SCB-DESC-000308          PIC X(18) VALUE 'DESC-004004'.
           05  SCB-GRP-0029.
               10  SCB-E-000309             OCCURS 3 TIMES.
                   15  SCB-EK-000309        PIC X(10).
                   15  SCB-EV-000309        PIC 9(06) COMP.
               10  SCB-A-000310             PIC 9(09) COMP VALUE 310.
               10  SCB-B-000311             PIC S9(07)V99 COMP-3 VALUE +0000311.
               10  SCB-C-000312             PIC X(24) VALUE 'FIELD-000312-ALPHA'.
               10  SCB-D-000313             PIC 9(04) VALUE 2191.
               10  SCB-DESC-000313          PIC X(18) VALUE 'DESC-004069'.
               10  SCB-E-000314             OCCURS 4 TIMES.
                   15  SCB-EK-000314        PIC X(10).
                   15  SCB-EV-000314        PIC 9(06) COMP.
               10  SCB-A-000315             PIC 9(09) COMP VALUE 315.
               10  SCB-B-000316             PIC S9(07)V99 COMP-3 VALUE +0000316.
               10  SCB-C-000317             PIC X(24) VALUE 'FIELD-000317-ALPHA'.
           05  SCB-GRP-0030.
               10  SCB-D-000318             PIC 9(04) VALUE 2226.
               10  SCB-DESC-000318          PIC X(18) VALUE 'DESC-004134'.
               10  SCB-E-000319             OCCURS 5 TIMES.
                   15  SCB-EK-000319        PIC X(10).
                   15  SCB-EV-000319        PIC 9(06) COMP.
               10  SCB-A-000320             PIC 9(09) COMP VALUE 320.
               10  SCB-B-000321             PIC S9(07)V99 COMP-3 VALUE +0000321.
               10  SCB-C-000322             PIC X(24) VALUE 'FIELD-000322-ALPHA'.
               10  SCB-D-000323             PIC 9(04) VALUE 2261.
               10  SCB-DESC-000323          PIC X(18) VALUE 'DESC-004199'.
               10  SCB-E-000324             OCCURS 2 TIMES.
                   15  SCB-EK-000324        PIC X(10).
                   15  SCB-EV-000324        PIC 9(06) COMP.
               10  SCB-A-000325             PIC 9(09) COMP VALUE 325.
               10  SCB-B-000326             PIC S9(07)V99 COMP-3 VALUE +0000326.
               10  SCB-C-000327             PIC X(24) VALUE 'FIELD-000327-ALPHA'.
           05  SCB-ALT-0005 REDEFINES SCB-GRP-0030.
               10  SCB-ALT-FLAG-0005      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0005   PIC X(64).
           05  SCB-GRP-0031.
               10  SCB-D-000328             PIC 9(04) VALUE 2296.
               10  SCB-DESC-000328          PIC X(18) VALUE 'DESC-004264'.
               10  SCB-E-000329             OCCURS 3 TIMES.
                   15  SCB-EK-000329        PIC X(10).
                   15  SCB-EV-000329        PIC 9(06) COMP.
               10  SCB-A-000330             PIC 9(09) COMP VALUE 330.
               10  SCB-B-000331             PIC S9(07)V99 COMP-3 VALUE +0000331.
               10  SCB-C-000332             PIC X(24) VALUE 'FIELD-000332-ALPHA'.
               10  SCB-D-000333             PIC 9(04) VALUE 2331.
               10  SCB-DESC-000333          PIC X(18) VALUE 'DESC-004329'.
               10  SCB-E-000334             OCCURS 4 TIMES.
                   15  SCB-EK-000334        PIC X(10).
                   15  SCB-EV-000334        PIC 9(06) COMP.
               10  SCB-A-000335             PIC 9(09) COMP VALUE 335.
               10  SCB-B-000336             PIC S9(07)V99 COMP-3 VALUE +0000336.
               10  SCB-C-000337             PIC X(24) VALUE 'FIELD-000337-ALPHA'.
               10  SCB-D-000338             PIC 9(04) VALUE 2366.
               10  SCB-DESC-000338          PIC X(18) VALUE 'DESC-004394'.
           05  SCB-GRP-0032.
               10  SCB-E-000339             OCCURS 5 TIMES.
                   15  SCB-EK-000339        PIC X(10).
                   15  SCB-EV-000339        PIC 9(06) COMP.
               10  SCB-A-000340             PIC 9(09) COMP VALUE 340.
               10  SCB-B-000341             PIC S9(07)V99 COMP-3 VALUE +0000341.
               10  SCB-C-000342             PIC X(24) VALUE 'FIELD-000342-ALPHA'.
               10  SCB-D-000343             PIC 9(04) VALUE 2401.
               10  SCB-DESC-000343          PIC X(18) VALUE 'DESC-004459'.
               10  SCB-E-000344             OCCURS 2 TIMES.
                   15  SCB-EK-000344        PIC X(10).
                   15  SCB-EV-000344        PIC 9(06) COMP.
               10  SCB-A-000345             PIC 9(09) COMP VALUE 345.
               10  SCB-B-000346             PIC S9(07)V99 COMP-3 VALUE +0000346.
               10  SCB-C-000347             PIC X(24) VALUE 'FIELD-000347-ALPHA'.
               10  SCB-D-000348             PIC 9(04) VALUE 2436.
               10  SCB-DESC-000348          PIC X(18) VALUE 'DESC-004524'.
               10  SCB-E-000349             OCCURS 3 TIMES.
                   15  SCB-EK-000349        PIC X(10).
                   15  SCB-EV-000349        PIC 9(06) COMP.
               10  SCB-A-000350             PIC 9(09) COMP VALUE 350.
           05  SCB-GRP-0033.
               10  SCB-B-000351             PIC S9(07)V99 COMP-3 VALUE +0000351.
               10  SCB-C-000352             PIC X(24) VALUE 'FIELD-000352-ALPHA'.
               10  SCB-D-000353             PIC 9(04) VALUE 2471.
               10  SCB-DESC-000353          PIC X(18) VALUE 'DESC-004589'.
               10  SCB-E-000354             OCCURS 4 TIMES.
                   15  SCB-EK-000354        PIC X(10).
                   15  SCB-EV-000354        PIC 9(06) COMP.
               10  SCB-A-000355             PIC 9(09) COMP VALUE 355.
               10  SCB-B-000356             PIC S9(07)V99 COMP-3 VALUE +0000356.
               10  SCB-C-000357             PIC X(24) VALUE 'FIELD-000357-ALPHA'.
               10  SCB-D-000358             PIC 9(04) VALUE 2506.
               10  SCB-DESC-000358          PIC X(18) VALUE 'DESC-004654'.
               10  SCB-E-000359             OCCURS 5 TIMES.
                   15  SCB-EK-000359        PIC X(10).
                   15  SCB-EV-000359        PIC 9(06) COMP.
               10  SCB-A-000360             PIC 9(09) COMP VALUE 360.
               10  SCB-B-000361             PIC S9(07)V99 COMP-3 VALUE +0000361.
               10  SCB-C-000362             PIC X(24) VALUE 'FIELD-000362-ALPHA'.
               10  SCB-D-000363             PIC 9(04) VALUE 2541.
               10  SCB-DESC-000363          PIC X(18) VALUE 'DESC-004719'.
           05  SCB-GRP-0034.
               10  SCB-E-000364             OCCURS 2 TIMES.
                   15  SCB-EK-000364        PIC X(10).
                   15  SCB-EV-000364        PIC 9(06) COMP.
               10  SCB-A-000365             PIC 9(09) COMP VALUE 365.
               10  SCB-B-000366             PIC S9(07)V99 COMP-3 VALUE +0000366.
               10  SCB-C-000367             PIC X(24) VALUE 'FIELD-000367-ALPHA'.
               10  SCB-D-000368             PIC 9(04) VALUE 2576.
               10  SCB-DESC-000368          PIC X(18) VALUE 'DESC-004784'.
               10  SCB-E-000369             OCCURS 3 TIMES.
                   15  SCB-EK-000369        PIC X(10).
                   15  SCB-EV-000369        PIC 9(06) COMP.
               10  SCB-A-000370             PIC 9(09) COMP VALUE 370.
               10  SCB-B-000371             PIC S9(07)V99 COMP-3 VALUE +0000371.
               10  SCB-C-000372             PIC X(24) VALUE 'FIELD-000372-ALPHA'.
               10  SCB-D-000373             PIC 9(04) VALUE 2611.
               10  SCB-DESC-000373          PIC X(18) VALUE 'DESC-004849'.
               10  SCB-E-000374             OCCURS 4 TIMES.
                   15  SCB-EK-000374        PIC X(10).
                   15  SCB-EV-000374        PIC 9(06) COMP.
               10  SCB-A-000375             PIC 9(09) COMP VALUE 375.
               10  SCB-B-000376             PIC S9(07)V99 COMP-3 VALUE +0000376.
               10  SCB-C-000377             PIC X(24) VALUE 'FIELD-000377-ALPHA'.
           05  SCB-GRP-0035.
               10  SCB-D-000378             PIC 9(04) VALUE 2646.
               10  SCB-DESC-000378          PIC X(18) VALUE 'DESC-004914'.
               10  SCB-E-000379             OCCURS 5 TIMES.
                   15  SCB-EK-000379        PIC X(10).
                   15  SCB-EV-000379        PIC 9(06) COMP.
               10  SCB-A-000380             PIC 9(09) COMP VALUE 380.
               10  SCB-B-000381             PIC S9(07)V99 COMP-3 VALUE +0000381.
               10  SCB-C-000382             PIC X(24) VALUE 'FIELD-000382-ALPHA'.
               10  SCB-D-000383             PIC 9(04) VALUE 2681.
               10  SCB-DESC-000383          PIC X(18) VALUE 'DESC-004979'.
               10  SCB-E-000384             OCCURS 2 TIMES.
                   15  SCB-EK-000384        PIC X(10).
                   15  SCB-EV-000384        PIC 9(06) COMP.
               10  SCB-A-000385             PIC 9(09) COMP VALUE 385.
           05  SCB-GRP-0036.
               10  SCB-B-000386             PIC S9(07)V99 COMP-3 VALUE +0000386.
               10  SCB-C-000387             PIC X(24) VALUE 'FIELD-000387-ALPHA'.
               10  SCB-D-000388             PIC 9(04) VALUE 2716.
               10  SCB-DESC-000388          PIC X(18) VALUE 'DESC-005044'.
               10  SCB-E-000389             OCCURS 3 TIMES.
                   15  SCB-EK-000389        PIC X(10).
                   15  SCB-EV-000389        PIC 9(06) COMP.
               10  SCB-A-000390             PIC 9(09) COMP VALUE 390.
               10  SCB-B-000391             PIC S9(07)V99 COMP-3 VALUE +0000391.
               10  SCB-C-000392             PIC X(24) VALUE 'FIELD-000392-ALPHA'.
               10  SCB-D-000393             PIC 9(04) VALUE 2751.
               10  SCB-DESC-000393          PIC X(18) VALUE 'DESC-005109'.
               10  SCB-E-000394             OCCURS 4 TIMES.
                   15  SCB-EK-000394        PIC X(10).
                   15  SCB-EV-000394        PIC 9(06) COMP.
           05  SCB-ALT-0006 REDEFINES SCB-GRP-0036.
               10  SCB-ALT-FLAG-0006      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0006   PIC X(64).
           05  SCB-GRP-0037.
               10  SCB-A-000395             PIC 9(09) COMP VALUE 395.
               10  SCB-B-000396             PIC S9(07)V99 COMP-3 VALUE +0000396.
               10  SCB-C-000397             PIC X(24) VALUE 'FIELD-000397-ALPHA'.
               10  SCB-D-000398             PIC 9(04) VALUE 2786.
               10  SCB-DESC-000398          PIC X(18) VALUE 'DESC-005174'.
               10  SCB-E-000399             OCCURS 5 TIMES.
                   15  SCB-EK-000399        PIC X(10).
                   15  SCB-EV-000399        PIC 9(06) COMP.
               10  SCB-A-000400             PIC 9(09) COMP VALUE 400.
               10  SCB-B-000401             PIC S9(07)V99 COMP-3 VALUE +0000401.
               10  SCB-C-000402             PIC X(24) VALUE 'FIELD-000402-ALPHA'.
               10  SCB-D-000403             PIC 9(04) VALUE 2821.
               10  SCB-DESC-000403          PIC X(18) VALUE 'DESC-005239'.
               10  SCB-E-000404             OCCURS 2 TIMES.
                   15  SCB-EK-000404        PIC X(10).
                   15  SCB-EV-000404        PIC 9(06) COMP.
           05  SCB-GRP-0038.
               10  SCB-A-000405             PIC 9(09) COMP VALUE 405.
               10  SCB-B-000406             PIC S9(07)V99 COMP-3 VALUE +0000406.
               10  SCB-C-000407             PIC X(24) VALUE 'FIELD-000407-ALPHA'.
               10  SCB-D-000408             PIC 9(04) VALUE 2856.
               10  SCB-DESC-000408          PIC X(18) VALUE 'DESC-005304'.
               10  SCB-E-000409             OCCURS 3 TIMES.
                   15  SCB-EK-000409        PIC X(10).
                   15  SCB-EV-000409        PIC 9(06) COMP.
               10  SCB-A-000410             PIC 9(09) COMP VALUE 410.
               10  SCB-B-000411             PIC S9(07)V99 COMP-3 VALUE +0000411.
               10  SCB-C-000412             PIC X(24) VALUE 'FIELD-000412-ALPHA'.
               10  SCB-D-000413             PIC 9(04) VALUE 2891.
               10  SCB-DESC-000413          PIC X(18) VALUE 'DESC-005369'.
               10  SCB-E-000414             OCCURS 4 TIMES.
                   15  SCB-EK-000414        PIC X(10).
                   15  SCB-EV-000414        PIC 9(06) COMP.
               10  SCB-A-000415             PIC 9(09) COMP VALUE 415.
           05  SCB-GRP-0039.
               10  SCB-B-000416             PIC S9(07)V99 COMP-3 VALUE +0000416.
               10  SCB-C-000417             PIC X(24) VALUE 'FIELD-000417-ALPHA'.
               10  SCB-D-000418             PIC 9(04) VALUE 2926.
               10  SCB-DESC-000418          PIC X(18) VALUE 'DESC-005434'.
               10  SCB-E-000419             OCCURS 5 TIMES.
                   15  SCB-EK-000419        PIC X(10).
                   15  SCB-EV-000419        PIC 9(06) COMP.
               10  SCB-A-000420             PIC 9(09) COMP VALUE 420.
               10  SCB-B-000421             PIC S9(07)V99 COMP-3 VALUE +0000421.
               10  SCB-C-000422             PIC X(24) VALUE 'FIELD-000422-ALPHA'.
               10  SCB-D-000423             PIC 9(04) VALUE 2961.
               10  SCB-DESC-000423          PIC X(18) VALUE 'DESC-005499'.
               10  SCB-E-000424             OCCURS 2 TIMES.
                   15  SCB-EK-000424        PIC X(10).
                   15  SCB-EV-000424        PIC 9(06) COMP.
               10  SCB-A-000425             PIC 9(09) COMP VALUE 425.
               10  SCB-B-000426             PIC S9(07)V99 COMP-3 VALUE +0000426.
               10  SCB-C-000427             PIC X(24) VALUE 'FIELD-000427-ALPHA'.
           05  SCB-GRP-0040.
               10  SCB-D-000428             PIC 9(04) VALUE 2996.
               10  SCB-DESC-000428          PIC X(18) VALUE 'DESC-005564'.
               10  SCB-E-000429             OCCURS 3 TIMES.
                   15  SCB-EK-000429        PIC X(10).
                   15  SCB-EV-000429        PIC 9(06) COMP.
               10  SCB-A-000430             PIC 9(09) COMP VALUE 430.
               10  SCB-B-000431             PIC S9(07)V99 COMP-3 VALUE +0000431.
               10  SCB-C-000432             PIC X(24) VALUE 'FIELD-000432-ALPHA'.
               10  SCB-D-000433             PIC 9(04) VALUE 3031.
               10  SCB-DESC-000433          PIC X(18) VALUE 'DESC-005629'.
               10  SCB-E-000434             OCCURS 4 TIMES.
                   15  SCB-EK-000434        PIC X(10).
                   15  SCB-EV-000434        PIC 9(06) COMP.
               10  SCB-A-000435             PIC 9(09) COMP VALUE 435.
               10  SCB-B-000436             PIC S9(07)V99 COMP-3 VALUE +0000436.
               10  SCB-C-000437             PIC X(24) VALUE 'FIELD-000437-ALPHA'.
               10  SCB-D-000438             PIC 9(04) VALUE 3066.
               10  SCB-DESC-000438          PIC X(18) VALUE 'DESC-005694'.
               10  SCB-E-000439             OCCURS 5 TIMES.
                   15  SCB-EK-000439        PIC X(10).
                   15  SCB-EV-000439        PIC 9(06) COMP.
               10  SCB-A-000440             PIC 9(09) COMP VALUE 440.
           05  SCB-GRP-0041.
               10  SCB-B-000441             PIC S9(07)V99 COMP-3 VALUE +0000441.
               10  SCB-C-000442             PIC X(24) VALUE 'FIELD-000442-ALPHA'.
               10  SCB-D-000443             PIC 9(04) VALUE 3101.
               10  SCB-DESC-000443          PIC X(18) VALUE 'DESC-005759'.
               10  SCB-E-000444             OCCURS 2 TIMES.
                   15  SCB-EK-000444        PIC X(10).
                   15  SCB-EV-000444        PIC 9(06) COMP.
               10  SCB-A-000445             PIC 9(09) COMP VALUE 445.
               10  SCB-B-000446             PIC S9(07)V99 COMP-3 VALUE +0000446.
               10  SCB-C-000447             PIC X(24) VALUE 'FIELD-000447-ALPHA'.
               10  SCB-D-000448             PIC 9(04) VALUE 3136.
               10  SCB-DESC-000448          PIC X(18) VALUE 'DESC-005824'.
               10  SCB-E-000449             OCCURS 3 TIMES.
                   15  SCB-EK-000449        PIC X(10).
                   15  SCB-EV-000449        PIC 9(06) COMP.
               10  SCB-A-000450             PIC 9(09) COMP VALUE 450.
               10  SCB-B-000451             PIC S9(07)V99 COMP-3 VALUE +0000451.
               10  SCB-C-000452             PIC X(24) VALUE 'FIELD-000452-ALPHA'.
               10  SCB-D-000453             PIC 9(04) VALUE 3171.
               10  SCB-DESC-000453          PIC X(18) VALUE 'DESC-005889'.
               10  SCB-E-000454             OCCURS 4 TIMES.
                   15  SCB-EK-000454        PIC X(10).
                   15  SCB-EV-000454        PIC 9(06) COMP.
           05  SCB-GRP-0042.
               10  SCB-A-000455             PIC 9(09) COMP VALUE 455.
               10  SCB-B-000456             PIC S9(07)V99 COMP-3 VALUE +0000456.
               10  SCB-C-000457             PIC X(24) VALUE 'FIELD-000457-ALPHA'.
               10  SCB-D-000458             PIC 9(04) VALUE 3206.
               10  SCB-DESC-000458          PIC X(18) VALUE 'DESC-005954'.
               10  SCB-E-000459             OCCURS 5 TIMES.
                   15  SCB-EK-000459        PIC X(10).
                   15  SCB-EV-000459        PIC 9(06) COMP.
               10  SCB-A-000460             PIC 9(09) COMP VALUE 460.
               10  SCB-B-000461             PIC S9(07)V99 COMP-3 VALUE +0000461.
               10  SCB-C-000462             PIC X(24) VALUE 'FIELD-000462-ALPHA'.
           05  SCB-ALT-0007 REDEFINES SCB-GRP-0042.
               10  SCB-ALT-FLAG-0007      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0007   PIC X(64).
           05  SCB-GRP-0043.
               10  SCB-D-000463             PIC 9(04) VALUE 3241.
               10  SCB-DESC-000463          PIC X(18) VALUE 'DESC-006019'.
               10  SCB-E-000464             OCCURS 2 TIMES.
                   15  SCB-EK-000464        PIC X(10).
                   15  SCB-EV-000464        PIC 9(06) COMP.
               10  SCB-A-000465             PIC 9(09) COMP VALUE 465.
               10  SCB-B-000466             PIC S9(07)V99 COMP-3 VALUE +0000466.
               10  SCB-C-000467             PIC X(24) VALUE 'FIELD-000467-ALPHA'.
               10  SCB-D-000468             PIC 9(04) VALUE 3276.
               10  SCB-DESC-000468          PIC X(18) VALUE 'DESC-006084'.
               10  SCB-E-000469             OCCURS 3 TIMES.
                   15  SCB-EK-000469        PIC X(10).
                   15  SCB-EV-000469        PIC 9(06) COMP.
               10  SCB-A-000470             PIC 9(09) COMP VALUE 470.
               10  SCB-B-000471             PIC S9(07)V99 COMP-3 VALUE +0000471.
           05  SCB-GRP-0044.
               10  SCB-C-000472             PIC X(24) VALUE 'FIELD-000472-ALPHA'.
               10  SCB-D-000473             PIC 9(04) VALUE 3311.
               10  SCB-DESC-000473          PIC X(18) VALUE 'DESC-006149'.
               10  SCB-E-000474             OCCURS 4 TIMES.
                   15  SCB-EK-000474        PIC X(10).
                   15  SCB-EV-000474        PIC 9(06) COMP.
               10  SCB-A-000475             PIC 9(09) COMP VALUE 475.
               10  SCB-B-000476             PIC S9(07)V99 COMP-3 VALUE +0000476.
               10  SCB-C-000477             PIC X(24) VALUE 'FIELD-000477-ALPHA'.
               10  SCB-D-000478             PIC 9(04) VALUE 3346.
               10  SCB-DESC-000478          PIC X(18) VALUE 'DESC-006214'.
               10  SCB-E-000479             OCCURS 5 TIMES.
                   15  SCB-EK-000479        PIC X(10).
                   15  SCB-EV-000479        PIC 9(06) COMP.
               10  SCB-A-000480             PIC 9(09) COMP VALUE 480.
               10  SCB-B-000481             PIC S9(07)V99 COMP-3 VALUE +0000481.
           05  SCB-GRP-0045.
               10  SCB-C-000482             PIC X(24) VALUE 'FIELD-000482-ALPHA'.
               10  SCB-D-000483             PIC 9(04) VALUE 3381.
               10  SCB-DESC-000483          PIC X(18) VALUE 'DESC-006279'.
               10  SCB-E-000484             OCCURS 2 TIMES.
                   15  SCB-EK-000484        PIC X(10).
                   15  SCB-EV-000484        PIC 9(06) COMP.
               10  SCB-A-000485             PIC 9(09) COMP VALUE 485.
               10  SCB-B-000486             PIC S9(07)V99 COMP-3 VALUE +0000486.
               10  SCB-C-000487             PIC X(24) VALUE 'FIELD-000487-ALPHA'.
               10  SCB-D-000488             PIC 9(04) VALUE 3416.
               10  SCB-DESC-000488          PIC X(18) VALUE 'DESC-006344'.
               10  SCB-E-000489             OCCURS 3 TIMES.
                   15  SCB-EK-000489        PIC X(10).
                   15  SCB-EV-000489        PIC 9(06) COMP.
               10  SCB-A-000490             PIC 9(09) COMP VALUE 490.
               10  SCB-B-000491             PIC S9(07)V99 COMP-3 VALUE +0000491.
               10  SCB-C-000492             PIC X(24) VALUE 'FIELD-000492-ALPHA'.
           05  SCB-GRP-0046.
               10  SCB-D-000493             PIC 9(04) VALUE 3451.
               10  SCB-DESC-000493          PIC X(18) VALUE 'DESC-006409'.
               10  SCB-E-000494             OCCURS 4 TIMES.
                   15  SCB-EK-000494        PIC X(10).
                   15  SCB-EV-000494        PIC 9(06) COMP.
               10  SCB-A-000495             PIC 9(09) COMP VALUE 495.
               10  SCB-B-000496             PIC S9(07)V99 COMP-3 VALUE +0000496.
               10  SCB-C-000497             PIC X(24) VALUE 'FIELD-000497-ALPHA'.
               10  SCB-D-000498             PIC 9(04) VALUE 3486.
               10  SCB-DESC-000498          PIC X(18) VALUE 'DESC-006474'.
               10  SCB-E-000499             OCCURS 5 TIMES.
                   15  SCB-EK-000499        PIC X(10).
                   15  SCB-EV-000499        PIC 9(06) COMP.
               10  SCB-A-000500             PIC 9(09) COMP VALUE 500.
               10  SCB-B-000501             PIC S9(07)V99 COMP-3 VALUE +0000501.
               10  SCB-C-000502             PIC X(24) VALUE 'FIELD-000502-ALPHA'.
               10  SCB-D-000503             PIC 9(04) VALUE 3521.
               10  SCB-DESC-000503          PIC X(18) VALUE 'DESC-006539'.
               10  SCB-E-000504             OCCURS 2 TIMES.
                   15  SCB-EK-000504        PIC X(10).
                   15  SCB-EV-000504        PIC 9(06) COMP.
           05  SCB-GRP-0047.
               10  SCB-A-000505             PIC 9(09) COMP VALUE 505.
               10  SCB-B-000506             PIC S9(07)V99 COMP-3 VALUE +0000506.
               10  SCB-C-000507             PIC X(24) VALUE 'FIELD-000507-ALPHA'.
               10  SCB-D-000508             PIC 9(04) VALUE 3556.
               10  SCB-DESC-000508          PIC X(18) VALUE 'DESC-006604'.
               10  SCB-E-000509             OCCURS 3 TIMES.
                   15  SCB-EK-000509        PIC X(10).
                   15  SCB-EV-000509        PIC 9(06) COMP.
               10  SCB-A-000510             PIC 9(09) COMP VALUE 510.
               10  SCB-B-000511             PIC S9(07)V99 COMP-3 VALUE +0000511.
               10  SCB-C-000512             PIC X(24) VALUE 'FIELD-000512-ALPHA'.
               10  SCB-D-000513             PIC 9(04) VALUE 3591.
               10  SCB-DESC-000513          PIC X(18) VALUE 'DESC-006669'.
               10  SCB-E-000514             OCCURS 4 TIMES.
                   15  SCB-EK-000514        PIC X(10).
                   15  SCB-EV-000514        PIC 9(06) COMP.
               10  SCB-A-000515             PIC 9(09) COMP VALUE 515.
               10  SCB-B-000516             PIC S9(07)V99 COMP-3 VALUE +0000516.
               10  SCB-C-000517             PIC X(24) VALUE 'FIELD-000517-ALPHA'.
           05  SCB-GRP-0048.
               10  SCB-D-000518             PIC 9(04) VALUE 3626.
               10  SCB-DESC-000518          PIC X(18) VALUE 'DESC-006734'.
               10  SCB-E-000519             OCCURS 5 TIMES.
                   15  SCB-EK-000519        PIC X(10).
                   15  SCB-EV-000519        PIC 9(06) COMP.
               10  SCB-A-000520             PIC 9(09) COMP VALUE 520.
               10  SCB-B-000521             PIC S9(07)V99 COMP-3 VALUE +0000521.
               10  SCB-C-000522             PIC X(24) VALUE 'FIELD-000522-ALPHA'.
               10  SCB-D-000523             PIC 9(04) VALUE 3661.
               10  SCB-DESC-000523          PIC X(18) VALUE 'DESC-006799'.
               10  SCB-E-000524             OCCURS 2 TIMES.
                   15  SCB-EK-000524        PIC X(10).
                   15  SCB-EV-000524        PIC 9(06) COMP.
               10  SCB-A-000525             PIC 9(09) COMP VALUE 525.
               10  SCB-B-000526             PIC S9(07)V99 COMP-3 VALUE +0000526.
               10  SCB-C-000527             PIC X(24) VALUE 'FIELD-000527-ALPHA'.
               10  SCB-D-000528             PIC 9(04) VALUE 3696.
               10  SCB-DESC-000528          PIC X(18) VALUE 'DESC-006864'.
               10  SCB-E-000529             OCCURS 3 TIMES.
                   15  SCB-EK-000529        PIC X(10).
                   15  SCB-EV-000529        PIC 9(06) COMP.
               10  SCB-A-000530             PIC 9(09) COMP VALUE 530.
               10  SCB-B-000531             PIC S9(07)V99 COMP-3 VALUE +0000531.
           05  SCB-ALT-0008 REDEFINES SCB-GRP-0048.
               10  SCB-ALT-FLAG-0008      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0008   PIC X(64).
           05  SCB-GRP-0049.
               10  SCB-C-000532             PIC X(24) VALUE 'FIELD-000532-ALPHA'.
               10  SCB-D-000533             PIC 9(04) VALUE 3731.
               10  SCB-DESC-000533          PIC X(18) VALUE 'DESC-006929'.
               10  SCB-E-000534             OCCURS 4 TIMES.
                   15  SCB-EK-000534        PIC X(10).
                   15  SCB-EV-000534        PIC 9(06) COMP.
               10  SCB-A-000535             PIC 9(09) COMP VALUE 535.
               10  SCB-B-000536             PIC S9(07)V99 COMP-3 VALUE +0000536.
               10  SCB-C-000537             PIC X(24) VALUE 'FIELD-000537-ALPHA'.
               10  SCB-D-000538             PIC 9(04) VALUE 3766.
               10  SCB-DESC-000538          PIC X(18) VALUE 'DESC-006994'.
               10  SCB-E-000539             OCCURS 5 TIMES.
                   15  SCB-EK-000539        PIC X(10).
                   15  SCB-EV-000539        PIC 9(06) COMP.
           05  SCB-GRP-0050.
               10  SCB-A-000540             PIC 9(09) COMP VALUE 540.
               10  SCB-B-000541             PIC S9(07)V99 COMP-3 VALUE +0000541.
               10  SCB-C-000542             PIC X(24) VALUE 'FIELD-000542-ALPHA'.
               10  SCB-D-000543             PIC 9(04) VALUE 3801.
               10  SCB-DESC-000543          PIC X(18) VALUE 'DESC-007059'.
               10  SCB-E-000544             OCCURS 2 TIMES.
                   15  SCB-EK-000544        PIC X(10).
                   15  SCB-EV-000544        PIC 9(06) COMP.
               10  SCB-A-000545             PIC 9(09) COMP VALUE 545.
               10  SCB-B-000546             PIC S9(07)V99 COMP-3 VALUE +0000546.
               10  SCB-C-000547             PIC X(24) VALUE 'FIELD-000547-ALPHA'.
               10  SCB-D-000548             PIC 9(04) VALUE 3836.
               10  SCB-DESC-000548          PIC X(18) VALUE 'DESC-007124'.
           05  SCB-GRP-0051.
               10  SCB-E-000549             OCCURS 3 TIMES.
                   15  SCB-EK-000549        PIC X(10).
                   15  SCB-EV-000549        PIC 9(06) COMP.
               10  SCB-A-000550             PIC 9(09) COMP VALUE 550.
               10  SCB-B-000551             PIC S9(07)V99 COMP-3 VALUE +0000551.
               10  SCB-C-000552             PIC X(24) VALUE 'FIELD-000552-ALPHA'.
               10  SCB-D-000553             PIC 9(04) VALUE 3871.
               10  SCB-DESC-000553          PIC X(18) VALUE 'DESC-007189'.
               10  SCB-E-000554             OCCURS 4 TIMES.
                   15  SCB-EK-000554        PIC X(10).
                   15  SCB-EV-000554        PIC 9(06) COMP.
               10  SCB-A-000555             PIC 9(09) COMP VALUE 555.
               10  SCB-B-000556             PIC S9(07)V99 COMP-3 VALUE +0000556.
               10  SCB-C-000557             PIC X(24) VALUE 'FIELD-000557-ALPHA'.
               10  SCB-D-000558             PIC 9(04) VALUE 3906.
               10  SCB-DESC-000558          PIC X(18) VALUE 'DESC-007254'.
           05  SCB-GRP-0052.
               10  SCB-E-000559             OCCURS 5 TIMES.
                   15  SCB-EK-000559        PIC X(10).
                   15  SCB-EV-000559        PIC 9(06) COMP.
               10  SCB-A-000560             PIC 9(09) COMP VALUE 560.
               10  SCB-B-000561             PIC S9(07)V99 COMP-3 VALUE +0000561.
               10  SCB-C-000562             PIC X(24) VALUE 'FIELD-000562-ALPHA'.
               10  SCB-D-000563             PIC 9(04) VALUE 3941.
               10  SCB-DESC-000563          PIC X(18) VALUE 'DESC-007319'.
               10  SCB-E-000564             OCCURS 2 TIMES.
                   15  SCB-EK-000564        PIC X(10).
                   15  SCB-EV-000564        PIC 9(06) COMP.
               10  SCB-A-000565             PIC 9(09) COMP VALUE 565.
               10  SCB-B-000566             PIC S9(07)V99 COMP-3 VALUE +0000566.
               10  SCB-C-000567             PIC X(24) VALUE 'FIELD-000567-ALPHA'.
               10  SCB-D-000568             PIC 9(04) VALUE 3976.
               10  SCB-DESC-000568          PIC X(18) VALUE 'DESC-007384'.
               10  SCB-E-000569             OCCURS 3 TIMES.
                   15  SCB-EK-000569        PIC X(10).
                   15  SCB-EV-000569        PIC 9(06) COMP.
           05  SCB-GRP-0053.
               10  SCB-A-000570             PIC 9(09) COMP VALUE 570.
               10  SCB-B-000571             PIC S9(07)V99 COMP-3 VALUE +0000571.
               10  SCB-C-000572             PIC X(24) VALUE 'FIELD-000572-ALPHA'.
               10  SCB-D-000573             PIC 9(04) VALUE 4011.
               10  SCB-DESC-000573          PIC X(18) VALUE 'DESC-007449'.
               10  SCB-E-000574             OCCURS 4 TIMES.
                   15  SCB-EK-000574        PIC X(10).
                   15  SCB-EV-000574        PIC 9(06) COMP.
               10  SCB-A-000575             PIC 9(09) COMP VALUE 575.
               10  SCB-B-000576             PIC S9(07)V99 COMP-3 VALUE +0000576.
               10  SCB-C-000577             PIC X(24) VALUE 'FIELD-000577-ALPHA'.
               10  SCB-D-000578             PIC 9(04) VALUE 4046.
               10  SCB-DESC-000578          PIC X(18) VALUE 'DESC-007514'.
               10  SCB-E-000579             OCCURS 5 TIMES.
                   15  SCB-EK-000579        PIC X(10).
                   15  SCB-EV-000579        PIC 9(06) COMP.
               10  SCB-A-000580             PIC 9(09) COMP VALUE 580.
               10  SCB-B-000581             PIC S9(07)V99 COMP-3 VALUE +0000581.
           05  SCB-GRP-0054.
               10  SCB-C-000582             PIC X(24) VALUE 'FIELD-000582-ALPHA'.
               10  SCB-D-000583             PIC 9(04) VALUE 4081.
               10  SCB-DESC-000583          PIC X(18) VALUE 'DESC-007579'.
               10  SCB-E-000584             OCCURS 2 TIMES.
                   15  SCB-EK-000584        PIC X(10).
                   15  SCB-EV-000584        PIC 9(06) COMP.
               10  SCB-A-000585             PIC 9(09) COMP VALUE 585.
               10  SCB-B-000586             PIC S9(07)V99 COMP-3 VALUE +0000586.
               10  SCB-C-000587             PIC X(24) VALUE 'FIELD-000587-ALPHA'.
               10  SCB-D-000588             PIC 9(04) VALUE 4116.
               10  SCB-DESC-000588          PIC X(18) VALUE 'DESC-007644'.
               10  SCB-E-000589             OCCURS 3 TIMES.
                   15  SCB-EK-000589        PIC X(10).
                   15  SCB-EV-000589        PIC 9(06) COMP.
               10  SCB-A-000590             PIC 9(09) COMP VALUE 590.
               10  SCB-B-000591             PIC S9(07)V99 COMP-3 VALUE +0000591.
               10  SCB-C-000592             PIC X(24) VALUE 'FIELD-000592-ALPHA'.
               10  SCB-D-000593             PIC 9(04) VALUE 4151.
               10  SCB-DESC-000593          PIC X(18) VALUE 'DESC-007709'.
               10  SCB-E-000594             OCCURS 4 TIMES.
                   15  SCB-EK-000594        PIC X(10).
                   15  SCB-EV-000594        PIC 9(06) COMP.
           05  SCB-ALT-0009 REDEFINES SCB-GRP-0054.
               10  SCB-ALT-FLAG-0009      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0009   PIC X(64).
           05  SCB-GRP-0055.
               10  SCB-A-000595             PIC 9(09) COMP VALUE 595.
               10  SCB-B-000596             PIC S9(07)V99 COMP-3 VALUE +0000596.
               10  SCB-C-000597             PIC X(24) VALUE 'FIELD-000597-ALPHA'.
               10  SCB-D-000598             PIC 9(04) VALUE 4186.
               10  SCB-DESC-000598          PIC X(18) VALUE 'DESC-007774'.
               10  SCB-E-000599             OCCURS 5 TIMES.
                   15  SCB-EK-000599        PIC X(10).
                   15  SCB-EV-000599        PIC 9(06) COMP.
               10  SCB-A-000600             PIC 9(09) COMP VALUE 600.
               10  SCB-B-000601             PIC S9(07)V99 COMP-3 VALUE +0000601.
               10  SCB-C-000602             PIC X(24) VALUE 'FIELD-000602-ALPHA'.
               10  SCB-D-000603             PIC 9(04) VALUE 4221.
               10  SCB-DESC-000603          PIC X(18) VALUE 'DESC-007839'.
               10  SCB-E-000604             OCCURS 2 TIMES.
                   15  SCB-EK-000604        PIC X(10).
                   15  SCB-EV-000604        PIC 9(06) COMP.
               10  SCB-A-000605             PIC 9(09) COMP VALUE 605.
               10  SCB-B-000606             PIC S9(07)V99 COMP-3 VALUE +0000606.
               10  SCB-C-000607             PIC X(24) VALUE 'FIELD-000607-ALPHA'.
               10  SCB-D-000608             PIC 9(04) VALUE 4256.
               10  SCB-DESC-000608          PIC X(18) VALUE 'DESC-007904'.
           05  SCB-GRP-0056.
               10  SCB-E-000609             OCCURS 3 TIMES.
                   15  SCB-EK-000609        PIC X(10).
                   15  SCB-EV-000609        PIC 9(06) COMP.
               10  SCB-A-000610             PIC 9(09) COMP VALUE 610.
               10  SCB-B-000611             PIC S9(07)V99 COMP-3 VALUE +0000611.
               10  SCB-C-000612             PIC X(24) VALUE 'FIELD-000612-ALPHA'.
               10  SCB-D-000613             PIC 9(04) VALUE 4291.
               10  SCB-DESC-000613          PIC X(18) VALUE 'DESC-007969'.
               10  SCB-E-000614             OCCURS 4 TIMES.
                   15  SCB-EK-000614        PIC X(10).
                   15  SCB-EV-000614        PIC 9(06) COMP.
               10  SCB-A-000615             PIC 9(09) COMP VALUE 615.
               10  SCB-B-000616             PIC S9(07)V99 COMP-3 VALUE +0000616.
           05  SCB-GRP-0057.
               10  SCB-C-000617             PIC X(24) VALUE 'FIELD-000617-ALPHA'.
               10  SCB-D-000618             PIC 9(04) VALUE 4326.
               10  SCB-DESC-000618          PIC X(18) VALUE 'DESC-008034'.
               10  SCB-E-000619             OCCURS 5 TIMES.
                   15  SCB-EK-000619        PIC X(10).
                   15  SCB-EV-000619        PIC 9(06) COMP.
               10  SCB-A-000620             PIC 9(09) COMP VALUE 620.
               10  SCB-B-000621             PIC S9(07)V99 COMP-3 VALUE +0000621.
               10  SCB-C-000622             PIC X(24) VALUE 'FIELD-000622-ALPHA'.
               10  SCB-D-000623             PIC 9(04) VALUE 4361.
               10  SCB-DESC-000623          PIC X(18) VALUE 'DESC-008099'.
               10  SCB-E-000624             OCCURS 2 TIMES.
                   15  SCB-EK-000624        PIC X(10).
                   15  SCB-EV-000624        PIC 9(06) COMP.
               10  SCB-A-000625             PIC 9(09) COMP VALUE 625.
           05  SCB-GRP-0058.
               10  SCB-B-000626             PIC S9(07)V99 COMP-3 VALUE +0000626.
               10  SCB-C-000627             PIC X(24) VALUE 'FIELD-000627-ALPHA'.
               10  SCB-D-000628             PIC 9(04) VALUE 4396.
               10  SCB-DESC-000628          PIC X(18) VALUE 'DESC-008164'.
               10  SCB-E-000629             OCCURS 3 TIMES.
                   15  SCB-EK-000629        PIC X(10).
                   15  SCB-EV-000629        PIC 9(06) COMP.
               10  SCB-A-000630             PIC 9(09) COMP VALUE 630.
               10  SCB-B-000631             PIC S9(07)V99 COMP-3 VALUE +0000631.
               10  SCB-C-000632             PIC X(24) VALUE 'FIELD-000632-ALPHA'.
               10  SCB-D-000633             PIC 9(04) VALUE 4431.
               10  SCB-DESC-000633          PIC X(18) VALUE 'DESC-008229'.
               10  SCB-E-000634             OCCURS 4 TIMES.
                   15  SCB-EK-000634        PIC X(10).
                   15  SCB-EV-000634        PIC 9(06) COMP.
               10  SCB-A-000635             PIC 9(09) COMP VALUE 635.
           05  SCB-GRP-0059.
               10  SCB-B-000636             PIC S9(07)V99 COMP-3 VALUE +0000636.
               10  SCB-C-000637             PIC X(24) VALUE 'FIELD-000637-ALPHA'.
               10  SCB-D-000638             PIC 9(04) VALUE 4466.
               10  SCB-DESC-000638          PIC X(18) VALUE 'DESC-008294'.
               10  SCB-E-000639             OCCURS 5 TIMES.
                   15  SCB-EK-000639        PIC X(10).
                   15  SCB-EV-000639        PIC 9(06) COMP.
               10  SCB-A-000640             PIC 9(09) COMP VALUE 640.
               10  SCB-B-000641             PIC S9(07)V99 COMP-3 VALUE +0000641.
               10  SCB-C-000642             PIC X(24) VALUE 'FIELD-000642-ALPHA'.
               10  SCB-D-000643             PIC 9(04) VALUE 4501.
               10  SCB-DESC-000643          PIC X(18) VALUE 'DESC-008359'.
               10  SCB-E-000644             OCCURS 2 TIMES.
                   15  SCB-EK-000644        PIC X(10).
                   15  SCB-EV-000644        PIC 9(06) COMP.
               10  SCB-A-000645             PIC 9(09) COMP VALUE 645.
               10  SCB-B-000646             PIC S9(07)V99 COMP-3 VALUE +0000646.
           05  SCB-GRP-0060.
               10  SCB-C-000647             PIC X(24) VALUE 'FIELD-000647-ALPHA'.
               10  SCB-D-000648             PIC 9(04) VALUE 4536.
               10  SCB-DESC-000648          PIC X(18) VALUE 'DESC-008424'.
               10  SCB-E-000649             OCCURS 3 TIMES.
                   15  SCB-EK-000649        PIC X(10).
                   15  SCB-EV-000649        PIC 9(06) COMP.
               10  SCB-A-000650             PIC 9(09) COMP VALUE 650.
               10  SCB-B-000651             PIC S9(07)V99 COMP-3 VALUE +0000651.
               10  SCB-C-000652             PIC X(24) VALUE 'FIELD-000652-ALPHA'.
               10  SCB-D-000653             PIC 9(04) VALUE 4571.
               10  SCB-DESC-000653          PIC X(18) VALUE 'DESC-008489'.
               10  SCB-E-000654             OCCURS 4 TIMES.
                   15  SCB-EK-000654        PIC X(10).
                   15  SCB-EV-000654        PIC 9(06) COMP.
               10  SCB-A-000655             PIC 9(09) COMP VALUE 655.
               10  SCB-B-000656             PIC S9(07)V99 COMP-3 VALUE +0000656.
               10  SCB-C-000657             PIC X(24) VALUE 'FIELD-000657-ALPHA'.
               10  SCB-D-000658             PIC 9(04) VALUE 4606.
               10  SCB-DESC-000658          PIC X(18) VALUE 'DESC-008554'.
           05  SCB-ALT-0010 REDEFINES SCB-GRP-0060.
               10  SCB-ALT-FLAG-0010      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0010   PIC X(64).
           05  SCB-GRP-0061.
               10  SCB-E-000659             OCCURS 5 TIMES.
                   15  SCB-EK-000659        PIC X(10).
                   15  SCB-EV-000659        PIC 9(06) COMP.
               10  SCB-A-000660             PIC 9(09) COMP VALUE 660.
               10  SCB-B-000661             PIC S9(07)V99 COMP-3 VALUE +0000661.
               10  SCB-C-000662             PIC X(24) VALUE 'FIELD-000662-ALPHA'.
               10  SCB-D-000663             PIC 9(04) VALUE 4641.
               10  SCB-DESC-000663          PIC X(18) VALUE 'DESC-008619'.
               10  SCB-E-000664             OCCURS 2 TIMES.
                   15  SCB-EK-000664        PIC X(10).
                   15  SCB-EV-000664        PIC 9(06) COMP.
               10  SCB-A-000665             PIC 9(09) COMP VALUE 665.
               10  SCB-B-000666             PIC S9(07)V99 COMP-3 VALUE +0000666.
               10  SCB-C-000667             PIC X(24) VALUE 'FIELD-000667-ALPHA'.
               10  SCB-D-000668             PIC 9(04) VALUE 4676.
               10  SCB-DESC-000668          PIC X(18) VALUE 'DESC-008684'.
               10  SCB-E-000669             OCCURS 3 TIMES.
                   15  SCB-EK-000669        PIC X(10).
                   15  SCB-EV-000669        PIC 9(06) COMP.
               10  SCB-A-000670             PIC 9(09) COMP VALUE 670.
               10  SCB-B-000671             PIC S9(07)V99 COMP-3 VALUE +0000671.
           05  SCB-GRP-0062.
               10  SCB-C-000672             PIC X(24) VALUE 'FIELD-000672-ALPHA'.
               10  SCB-D-000673             PIC 9(04) VALUE 4711.
               10  SCB-DESC-000673          PIC X(18) VALUE 'DESC-008749'.
               10  SCB-E-000674             OCCURS 4 TIMES.
                   15  SCB-EK-000674        PIC X(10).
                   15  SCB-EV-000674        PIC 9(06) COMP.
               10  SCB-A-000675             PIC 9(09) COMP VALUE 675.
               10  SCB-B-000676             PIC S9(07)V99 COMP-3 VALUE +0000676.
               10  SCB-C-000677             PIC X(24) VALUE 'FIELD-000677-ALPHA'.
               10  SCB-D-000678             PIC 9(04) VALUE 4746.
               10  SCB-DESC-000678          PIC X(18) VALUE 'DESC-008814'.
               10  SCB-E-000679             OCCURS 5 TIMES.
                   15  SCB-EK-000679        PIC X(10).
                   15  SCB-EV-000679        PIC 9(06) COMP.
               10  SCB-A-000680             PIC 9(09) COMP VALUE 680.
               10  SCB-B-000681             PIC S9(07)V99 COMP-3 VALUE +0000681.
               10  SCB-C-000682             PIC X(24) VALUE 'FIELD-000682-ALPHA'.
               10  SCB-D-000683             PIC 9(04) VALUE 4781.
               10  SCB-DESC-000683          PIC X(18) VALUE 'DESC-008879'.
               10  SCB-E-000684             OCCURS 2 TIMES.
                   15  SCB-EK-000684        PIC X(10).
                   15  SCB-EV-000684        PIC 9(06) COMP.
               10  SCB-A-000685             PIC 9(09) COMP VALUE 685.
           05  SCB-GRP-0063.
               10  SCB-B-000686             PIC S9(07)V99 COMP-3 VALUE +0000686.
               10  SCB-C-000687             PIC X(24) VALUE 'FIELD-000687-ALPHA'.
               10  SCB-D-000688             PIC 9(04) VALUE 4816.
               10  SCB-DESC-000688          PIC X(18) VALUE 'DESC-008944'.
               10  SCB-E-000689             OCCURS 3 TIMES.
                   15  SCB-EK-000689        PIC X(10).
                   15  SCB-EV-000689        PIC 9(06) COMP.
               10  SCB-A-000690             PIC 9(09) COMP VALUE 690.
               10  SCB-B-000691             PIC S9(07)V99 COMP-3 VALUE +0000691.
               10  SCB-C-000692             PIC X(24) VALUE 'FIELD-000692-ALPHA'.
               10  SCB-D-000693             PIC 9(04) VALUE 4851.
               10  SCB-DESC-000693          PIC X(18) VALUE 'DESC-009009'.
           05  SCB-GRP-0064.
               10  SCB-E-000694             OCCURS 4 TIMES.
                   15  SCB-EK-000694        PIC X(10).
                   15  SCB-EV-000694        PIC 9(06) COMP.
               10  SCB-A-000695             PIC 9(09) COMP VALUE 695.
               10  SCB-B-000696             PIC S9(07)V99 COMP-3 VALUE +0000696.
               10  SCB-C-000697             PIC X(24) VALUE 'FIELD-000697-ALPHA'.
               10  SCB-D-000698             PIC 9(04) VALUE 4886.
               10  SCB-DESC-000698          PIC X(18) VALUE 'DESC-009074'.
               10  SCB-E-000699             OCCURS 5 TIMES.
                   15  SCB-EK-000699        PIC X(10).
                   15  SCB-EV-000699        PIC 9(06) COMP.
               10  SCB-A-000700             PIC 9(09) COMP VALUE 700.
               10  SCB-B-000701             PIC S9(07)V99 COMP-3 VALUE +0000701.
               10  SCB-C-000702             PIC X(24) VALUE 'FIELD-000702-ALPHA'.
           05  SCB-GRP-0065.
               10  SCB-D-000703             PIC 9(04) VALUE 4921.
               10  SCB-DESC-000703          PIC X(18) VALUE 'DESC-009139'.
               10  SCB-E-000704             OCCURS 2 TIMES.
                   15  SCB-EK-000704        PIC X(10).
                   15  SCB-EV-000704        PIC 9(06) COMP.
               10  SCB-A-000705             PIC 9(09) COMP VALUE 705.
               10  SCB-B-000706             PIC S9(07)V99 COMP-3 VALUE +0000706.
               10  SCB-C-000707             PIC X(24) VALUE 'FIELD-000707-ALPHA'.
               10  SCB-D-000708             PIC 9(04) VALUE 4956.
               10  SCB-DESC-000708          PIC X(18) VALUE 'DESC-009204'.
               10  SCB-E-000709             OCCURS 3 TIMES.
                   15  SCB-EK-000709        PIC X(10).
                   15  SCB-EV-000709        PIC 9(06) COMP.
               10  SCB-A-000710             PIC 9(09) COMP VALUE 710.
               10  SCB-B-000711             PIC S9(07)V99 COMP-3 VALUE +0000711.
               10  SCB-C-000712             PIC X(24) VALUE 'FIELD-000712-ALPHA'.
           05  SCB-GRP-0066.
               10  SCB-D-000713             PIC 9(04) VALUE 4991.
               10  SCB-DESC-000713          PIC X(18) VALUE 'DESC-009269'.
               10  SCB-E-000714             OCCURS 4 TIMES.
                   15  SCB-EK-000714        PIC X(10).
                   15  SCB-EV-000714        PIC 9(06) COMP.
               10  SCB-A-000715             PIC 9(09) COMP VALUE 715.
               10  SCB-B-000716             PIC S9(07)V99 COMP-3 VALUE +0000716.
               10  SCB-C-000717             PIC X(24) VALUE 'FIELD-000717-ALPHA'.
               10  SCB-D-000718             PIC 9(04) VALUE 5026.
               10  SCB-DESC-000718          PIC X(18) VALUE 'DESC-009334'.
               10  SCB-E-000719             OCCURS 5 TIMES.
                   15  SCB-EK-000719        PIC X(10).
                   15  SCB-EV-000719        PIC 9(06) COMP.
               10  SCB-A-000720             PIC 9(09) COMP VALUE 720.
               10  SCB-B-000721             PIC S9(07)V99 COMP-3 VALUE +0000721.
               10  SCB-C-000722             PIC X(24) VALUE 'FIELD-000722-ALPHA'.
               10  SCB-D-000723             PIC 9(04) VALUE 5061.
               10  SCB-DESC-000723          PIC X(18) VALUE 'DESC-009399'.
           05  SCB-ALT-0011 REDEFINES SCB-GRP-0066.
               10  SCB-ALT-FLAG-0011      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0011   PIC X(64).
           05  SCB-GRP-0067.
               10  SCB-E-000724             OCCURS 2 TIMES.
                   15  SCB-EK-000724        PIC X(10).
                   15  SCB-EV-000724        PIC 9(06) COMP.
               10  SCB-A-000725             PIC 9(09) COMP VALUE 725.
               10  SCB-B-000726             PIC S9(07)V99 COMP-3 VALUE +0000726.
               10  SCB-C-000727             PIC X(24) VALUE 'FIELD-000727-ALPHA'.
               10  SCB-D-000728             PIC 9(04) VALUE 5096.
               10  SCB-DESC-000728          PIC X(18) VALUE 'DESC-009464'.
               10  SCB-E-000729             OCCURS 3 TIMES.
                   15  SCB-EK-000729        PIC X(10).
                   15  SCB-EV-000729        PIC 9(06) COMP.
               10  SCB-A-000730             PIC 9(09) COMP VALUE 730.
               10  SCB-B-000731             PIC S9(07)V99 COMP-3 VALUE +0000731.
               10  SCB-C-000732             PIC X(24) VALUE 'FIELD-000732-ALPHA'.
               10  SCB-D-000733             PIC 9(04) VALUE 5131.
               10  SCB-DESC-000733          PIC X(18) VALUE 'DESC-009529'.
               10  SCB-E-000734             OCCURS 4 TIMES.
                   15  SCB-EK-000734        PIC X(10).
                   15  SCB-EV-000734        PIC 9(06) COMP.
               10  SCB-A-000735             PIC 9(09) COMP VALUE 735.
           05  SCB-GRP-0068.
               10  SCB-B-000736             PIC S9(07)V99 COMP-3 VALUE +0000736.
               10  SCB-C-000737             PIC X(24) VALUE 'FIELD-000737-ALPHA'.
               10  SCB-D-000738             PIC 9(04) VALUE 5166.
               10  SCB-DESC-000738          PIC X(18) VALUE 'DESC-009594'.
               10  SCB-E-000739             OCCURS 5 TIMES.
                   15  SCB-EK-000739        PIC X(10).
                   15  SCB-EV-000739        PIC 9(06) COMP.
               10  SCB-A-000740             PIC 9(09) COMP VALUE 740.
               10  SCB-B-000741             PIC S9(07)V99 COMP-3 VALUE +0000741.
               10  SCB-C-000742             PIC X(24) VALUE 'FIELD-000742-ALPHA'.
               10  SCB-D-000743             PIC 9(04) VALUE 5201.
               10  SCB-DESC-000743          PIC X(18) VALUE 'DESC-009659'.
               10  SCB-E-000744             OCCURS 2 TIMES.
                   15  SCB-EK-000744        PIC X(10).
                   15  SCB-EV-000744        PIC 9(06) COMP.
               10  SCB-A-000745             PIC 9(09) COMP VALUE 745.
               10  SCB-B-000746             PIC S9(07)V99 COMP-3 VALUE +0000746.
               10  SCB-C-000747             PIC X(24) VALUE 'FIELD-000747-ALPHA'.
               10  SCB-D-000748             PIC 9(04) VALUE 5236.
               10  SCB-DESC-000748          PIC X(18) VALUE 'DESC-009724'.
           05  SCB-GRP-0069.
               10  SCB-E-000749             OCCURS 3 TIMES.
                   15  SCB-EK-000749        PIC X(10).
                   15  SCB-EV-000749        PIC 9(06) COMP.
               10  SCB-A-000750             PIC 9(09) COMP VALUE 750.
               10  SCB-B-000751             PIC S9(07)V99 COMP-3 VALUE +0000751.
               10  SCB-C-000752             PIC X(24) VALUE 'FIELD-000752-ALPHA'.
               10  SCB-D-000753             PIC 9(04) VALUE 5271.
               10  SCB-DESC-000753          PIC X(18) VALUE 'DESC-009789'.
               10  SCB-E-000754             OCCURS 4 TIMES.
                   15  SCB-EK-000754        PIC X(10).
                   15  SCB-EV-000754        PIC 9(06) COMP.
               10  SCB-A-000755             PIC 9(09) COMP VALUE 755.
               10  SCB-B-000756             PIC S9(07)V99 COMP-3 VALUE +0000756.
               10  SCB-C-000757             PIC X(24) VALUE 'FIELD-000757-ALPHA'.
               10  SCB-D-000758             PIC 9(04) VALUE 5306.
               10  SCB-DESC-000758          PIC X(18) VALUE 'DESC-009854'.
               10  SCB-E-000759             OCCURS 5 TIMES.
                   15  SCB-EK-000759        PIC X(10).
                   15  SCB-EV-000759        PIC 9(06) COMP.
               10  SCB-A-000760             PIC 9(09) COMP VALUE 760.
               10  SCB-B-000761             PIC S9(07)V99 COMP-3 VALUE +0000761.
               10  SCB-C-000762             PIC X(24) VALUE 'FIELD-000762-ALPHA'.
           05  SCB-GRP-0070.
               10  SCB-D-000763             PIC 9(04) VALUE 5341.
               10  SCB-DESC-000763          PIC X(18) VALUE 'DESC-009919'.
               10  SCB-E-000764             OCCURS 2 TIMES.
                   15  SCB-EK-000764        PIC X(10).
                   15  SCB-EV-000764        PIC 9(06) COMP.
               10  SCB-A-000765             PIC 9(09) COMP VALUE 765.
               10  SCB-B-000766             PIC S9(07)V99 COMP-3 VALUE +0000766.
               10  SCB-C-000767             PIC X(24) VALUE 'FIELD-000767-ALPHA'.
               10  SCB-D-000768             PIC 9(04) VALUE 5376.
               10  SCB-DESC-000768          PIC X(18) VALUE 'DESC-009984'.
               10  SCB-E-000769             OCCURS 3 TIMES.
                   15  SCB-EK-000769        PIC X(10).
                   15  SCB-EV-000769        PIC 9(06) COMP.
               10  SCB-A-000770             PIC 9(09) COMP VALUE 770.
           05  SCB-GRP-0071.
               10  SCB-B-000771             PIC S9(07)V99 COMP-3 VALUE +0000771.
               10  SCB-C-000772             PIC X(24) VALUE 'FIELD-000772-ALPHA'.
               10  SCB-D-000773             PIC 9(04) VALUE 5411.
               10  SCB-DESC-000773          PIC X(18) VALUE 'DESC-010049'.
               10  SCB-E-000774             OCCURS 4 TIMES.
                   15  SCB-EK-000774        PIC X(10).
                   15  SCB-EV-000774        PIC 9(06) COMP.
               10  SCB-A-000775             PIC 9(09) COMP VALUE 775.
               10  SCB-B-000776             PIC S9(07)V99 COMP-3 VALUE +0000776.
               10  SCB-C-000777             PIC X(24) VALUE 'FIELD-000777-ALPHA'.
               10  SCB-D-000778             PIC 9(04) VALUE 5446.
               10  SCB-DESC-000778          PIC X(18) VALUE 'DESC-010114'.
               10  SCB-E-000779             OCCURS 5 TIMES.
                   15  SCB-EK-000779        PIC X(10).
                   15  SCB-EV-000779        PIC 9(06) COMP.
           05  SCB-GRP-0072.
               10  SCB-A-000780             PIC 9(09) COMP VALUE 780.
               10  SCB-B-000781             PIC S9(07)V99 COMP-3 VALUE +0000781.
               10  SCB-C-000782             PIC X(24) VALUE 'FIELD-000782-ALPHA'.
               10  SCB-D-000783             PIC 9(04) VALUE 5481.
               10  SCB-DESC-000783          PIC X(18) VALUE 'DESC-010179'.
               10  SCB-E-000784             OCCURS 2 TIMES.
                   15  SCB-EK-000784        PIC X(10).
                   15  SCB-EV-000784        PIC 9(06) COMP.
               10  SCB-A-000785             PIC 9(09) COMP VALUE 785.
               10  SCB-B-000786             PIC S9(07)V99 COMP-3 VALUE +0000786.
               10  SCB-C-000787             PIC X(24) VALUE 'FIELD-000787-ALPHA'.
               10  SCB-D-000788             PIC 9(04) VALUE 5516.
               10  SCB-DESC-000788          PIC X(18) VALUE 'DESC-010244'.
               10  SCB-E-000789             OCCURS 3 TIMES.
                   15  SCB-EK-000789        PIC X(10).
                   15  SCB-EV-000789        PIC 9(06) COMP.
           05  SCB-ALT-0012 REDEFINES SCB-GRP-0072.
               10  SCB-ALT-FLAG-0012      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0012   PIC X(64).
           05  SCB-GRP-0073.
               10  SCB-A-000790             PIC 9(09) COMP VALUE 790.
               10  SCB-B-000791             PIC S9(07)V99 COMP-3 VALUE +0000791.
               10  SCB-C-000792             PIC X(24) VALUE 'FIELD-000792-ALPHA'.
               10  SCB-D-000793             PIC 9(04) VALUE 5551.
               10  SCB-DESC-000793          PIC X(18) VALUE 'DESC-010309'.
               10  SCB-E-000794             OCCURS 4 TIMES.
                   15  SCB-EK-000794        PIC X(10).
                   15  SCB-EV-000794        PIC 9(06) COMP.
               10  SCB-A-000795             PIC 9(09) COMP VALUE 795.
               10  SCB-B-000796             PIC S9(07)V99 COMP-3 VALUE +0000796.
               10  SCB-C-000797             PIC X(24) VALUE 'FIELD-000797-ALPHA'.
               10  SCB-D-000798             PIC 9(04) VALUE 5586.
               10  SCB-DESC-000798          PIC X(18) VALUE 'DESC-010374'.
               10  SCB-E-000799             OCCURS 5 TIMES.
                   15  SCB-EK-000799        PIC X(10).
                   15  SCB-EV-000799        PIC 9(06) COMP.
               10  SCB-A-000800             PIC 9(09) COMP VALUE 800.
           05  SCB-GRP-0074.
               10  SCB-B-000801             PIC S9(07)V99 COMP-3 VALUE +0000801.
               10  SCB-C-000802             PIC X(24) VALUE 'FIELD-000802-ALPHA'.
               10  SCB-D-000803             PIC 9(04) VALUE 5621.
               10  SCB-DESC-000803          PIC X(18) VALUE 'DESC-010439'.
               10  SCB-E-000804             OCCURS 2 TIMES.
                   15  SCB-EK-000804        PIC X(10).
                   15  SCB-EV-000804        PIC 9(06) COMP.
               10  SCB-A-000805             PIC 9(09) COMP VALUE 805.
               10  SCB-B-000806             PIC S9(07)V99 COMP-3 VALUE +0000806.
               10  SCB-C-000807             PIC X(24) VALUE 'FIELD-000807-ALPHA'.
               10  SCB-D-000808             PIC 9(04) VALUE 5656.
               10  SCB-DESC-000808          PIC X(18) VALUE 'DESC-010504'.
               10  SCB-E-000809             OCCURS 3 TIMES.
                   15  SCB-EK-000809        PIC X(10).
                   15  SCB-EV-000809        PIC 9(06) COMP.
               10  SCB-A-000810             PIC 9(09) COMP VALUE 810.
               10  SCB-B-000811             PIC S9(07)V99 COMP-3 VALUE +0000811.
               10  SCB-C-000812             PIC X(24) VALUE 'FIELD-000812-ALPHA'.
           05  SCB-GRP-0075.
               10  SCB-D-000813             PIC 9(04) VALUE 5691.
               10  SCB-DESC-000813          PIC X(18) VALUE 'DESC-010569'.
               10  SCB-E-000814             OCCURS 4 TIMES.
                   15  SCB-EK-000814        PIC X(10).
                   15  SCB-EV-000814        PIC 9(06) COMP.
               10  SCB-A-000815             PIC 9(09) COMP VALUE 815.
               10  SCB-B-000816             PIC S9(07)V99 COMP-3 VALUE +0000816.
               10  SCB-C-000817             PIC X(24) VALUE 'FIELD-000817-ALPHA'.
               10  SCB-D-000818             PIC 9(04) VALUE 5726.
               10  SCB-DESC-000818          PIC X(18) VALUE 'DESC-010634'.
               10  SCB-E-000819             OCCURS 5 TIMES.
                   15  SCB-EK-000819        PIC X(10).
                   15  SCB-EV-000819        PIC 9(06) COMP.
               10  SCB-A-000820             PIC 9(09) COMP VALUE 820.
               10  SCB-B-000821             PIC S9(07)V99 COMP-3 VALUE +0000821.
               10  SCB-C-000822             PIC X(24) VALUE 'FIELD-000822-ALPHA'.
               10  SCB-D-000823             PIC 9(04) VALUE 5761.
               10  SCB-DESC-000823          PIC X(18) VALUE 'DESC-010699'.
               10  SCB-E-000824             OCCURS 2 TIMES.
                   15  SCB-EK-000824        PIC X(10).
                   15  SCB-EV-000824        PIC 9(06) COMP.
               10  SCB-A-000825             PIC 9(09) COMP VALUE 825.
           05  SCB-GRP-0076.
               10  SCB-B-000826             PIC S9(07)V99 COMP-3 VALUE +0000826.
               10  SCB-C-000827             PIC X(24) VALUE 'FIELD-000827-ALPHA'.
               10  SCB-D-000828             PIC 9(04) VALUE 5796.
               10  SCB-DESC-000828          PIC X(18) VALUE 'DESC-010764'.
               10  SCB-E-000829             OCCURS 3 TIMES.
                   15  SCB-EK-000829        PIC X(10).
                   15  SCB-EV-000829        PIC 9(06) COMP.
               10  SCB-A-000830             PIC 9(09) COMP VALUE 830.
               10  SCB-B-000831             PIC S9(07)V99 COMP-3 VALUE +0000831.
               10  SCB-C-000832             PIC X(24) VALUE 'FIELD-000832-ALPHA'.
               10  SCB-D-000833             PIC 9(04) VALUE 5831.
               10  SCB-DESC-000833          PIC X(18) VALUE 'DESC-010829'.
               10  SCB-E-000834             OCCURS 4 TIMES.
                   15  SCB-EK-000834        PIC X(10).
                   15  SCB-EV-000834        PIC 9(06) COMP.
               10  SCB-A-000835             PIC 9(09) COMP VALUE 835.
               10  SCB-B-000836             PIC S9(07)V99 COMP-3 VALUE +0000836.
               10  SCB-C-000837             PIC X(24) VALUE 'FIELD-000837-ALPHA'.
               10  SCB-D-000838             PIC 9(04) VALUE 5866.
               10  SCB-DESC-000838          PIC X(18) VALUE 'DESC-010894'.
               10  SCB-E-000839             OCCURS 5 TIMES.
                   15  SCB-EK-000839        PIC X(10).
                   15  SCB-EV-000839        PIC 9(06) COMP.
           05  SCB-GRP-0077.
               10  SCB-A-000840             PIC 9(09) COMP VALUE 840.
               10  SCB-B-000841             PIC S9(07)V99 COMP-3 VALUE +0000841.
               10  SCB-C-000842             PIC X(24) VALUE 'FIELD-000842-ALPHA'.
               10  SCB-D-000843             PIC 9(04) VALUE 5901.
               10  SCB-DESC-000843          PIC X(18) VALUE 'DESC-010959'.
               10  SCB-E-000844             OCCURS 2 TIMES.
                   15  SCB-EK-000844        PIC X(10).
                   15  SCB-EV-000844        PIC 9(06) COMP.
               10  SCB-A-000845             PIC 9(09) COMP VALUE 845.
               10  SCB-B-000846             PIC S9(07)V99 COMP-3 VALUE +0000846.
               10  SCB-C-000847             PIC X(24) VALUE 'FIELD-000847-ALPHA'.
           05  SCB-GRP-0078.
               10  SCB-D-000848             PIC 9(04) VALUE 5936.
               10  SCB-DESC-000848          PIC X(18) VALUE 'DESC-011024'.
               10  SCB-E-000849             OCCURS 3 TIMES.
                   15  SCB-EK-000849        PIC X(10).
                   15  SCB-EV-000849        PIC 9(06) COMP.
               10  SCB-A-000850             PIC 9(09) COMP VALUE 850.
               10  SCB-B-000851             PIC S9(07)V99 COMP-3 VALUE +0000851.
               10  SCB-C-000852             PIC X(24) VALUE 'FIELD-000852-ALPHA'.
               10  SCB-D-000853             PIC 9(04) VALUE 5971.
               10  SCB-DESC-000853          PIC X(18) VALUE 'DESC-011089'.
               10  SCB-E-000854             OCCURS 4 TIMES.
                   15  SCB-EK-000854        PIC X(10).
                   15  SCB-EV-000854        PIC 9(06) COMP.
               10  SCB-A-000855             PIC 9(09) COMP VALUE 855.
               10  SCB-B-000856             PIC S9(07)V99 COMP-3 VALUE +0000856.
           05  SCB-ALT-0013 REDEFINES SCB-GRP-0078.
               10  SCB-ALT-FLAG-0013      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0013   PIC X(64).
           05  SCB-GRP-0079.
               10  SCB-C-000857             PIC X(24) VALUE 'FIELD-000857-ALPHA'.
               10  SCB-D-000858             PIC 9(04) VALUE 6006.
               10  SCB-DESC-000858          PIC X(18) VALUE 'DESC-011154'.
               10  SCB-E-000859             OCCURS 5 TIMES.
                   15  SCB-EK-000859        PIC X(10).
                   15  SCB-EV-000859        PIC 9(06) COMP.
               10  SCB-A-000860             PIC 9(09) COMP VALUE 860.
               10  SCB-B-000861             PIC S9(07)V99 COMP-3 VALUE +0000861.
               10  SCB-C-000862             PIC X(24) VALUE 'FIELD-000862-ALPHA'.
               10  SCB-D-000863             PIC 9(04) VALUE 6041.
               10  SCB-DESC-000863          PIC X(18) VALUE 'DESC-011219'.
               10  SCB-E-000864             OCCURS 2 TIMES.
                   15  SCB-EK-000864        PIC X(10).
                   15  SCB-EV-000864        PIC 9(06) COMP.
               10  SCB-A-000865             PIC 9(09) COMP VALUE 865.
               10  SCB-B-000866             PIC S9(07)V99 COMP-3 VALUE +0000866.
           05  SCB-GRP-0080.
               10  SCB-C-000867             PIC X(24) VALUE 'FIELD-000867-ALPHA'.
               10  SCB-D-000868             PIC 9(04) VALUE 6076.
               10  SCB-DESC-000868          PIC X(18) VALUE 'DESC-011284'.
               10  SCB-E-000869             OCCURS 3 TIMES.
                   15  SCB-EK-000869        PIC X(10).
                   15  SCB-EV-000869        PIC 9(06) COMP.
               10  SCB-A-000870             PIC 9(09) COMP VALUE 870.
               10  SCB-B-000871             PIC S9(07)V99 COMP-3 VALUE +0000871.
               10  SCB-C-000872             PIC X(24) VALUE 'FIELD-000872-ALPHA'.
               10  SCB-D-000873             PIC 9(04) VALUE 6111.
               10  SCB-DESC-000873          PIC X(18) VALUE 'DESC-011349'.
               10  SCB-E-000874             OCCURS 4 TIMES.
                   15  SCB-EK-000874        PIC X(10).
                   15  SCB-EV-000874        PIC 9(06) COMP.
               10  SCB-A-000875             PIC 9(09) COMP VALUE 875.
               10  SCB-B-000876             PIC S9(07)V99 COMP-3 VALUE +0000876.
               10  SCB-C-000877             PIC X(24) VALUE 'FIELD-000877-ALPHA'.
           05  SCB-GRP-0081.
               10  SCB-D-000878             PIC 9(04) VALUE 6146.
               10  SCB-DESC-000878          PIC X(18) VALUE 'DESC-011414'.
               10  SCB-E-000879             OCCURS 5 TIMES.
                   15  SCB-EK-000879        PIC X(10).
                   15  SCB-EV-000879        PIC 9(06) COMP.
               10  SCB-A-000880             PIC 9(09) COMP VALUE 880.
               10  SCB-B-000881             PIC S9(07)V99 COMP-3 VALUE +0000881.
               10  SCB-C-000882             PIC X(24) VALUE 'FIELD-000882-ALPHA'.
               10  SCB-D-000883             PIC 9(04) VALUE 6181.
               10  SCB-DESC-000883          PIC X(18) VALUE 'DESC-011479'.
               10  SCB-E-000884             OCCURS 2 TIMES.
                   15  SCB-EK-000884        PIC X(10).
                   15  SCB-EV-000884        PIC 9(06) COMP.
               10  SCB-A-000885             PIC 9(09) COMP VALUE 885.
               10  SCB-B-000886             PIC S9(07)V99 COMP-3 VALUE +0000886.
               10  SCB-C-000887             PIC X(24) VALUE 'FIELD-000887-ALPHA'.
               10  SCB-D-000888             PIC 9(04) VALUE 6216.
               10  SCB-DESC-000888          PIC X(18) VALUE 'DESC-011544'.
               10  SCB-E-000889             OCCURS 3 TIMES.
                   15  SCB-EK-000889        PIC X(10).
                   15  SCB-EV-000889        PIC 9(06) COMP.
           05  SCB-GRP-0082.
               10  SCB-A-000890             PIC 9(09) COMP VALUE 890.
               10  SCB-B-000891             PIC S9(07)V99 COMP-3 VALUE +0000891.
               10  SCB-C-000892             PIC X(24) VALUE 'FIELD-000892-ALPHA'.
               10  SCB-D-000893             PIC 9(04) VALUE 6251.
               10  SCB-DESC-000893          PIC X(18) VALUE 'DESC-011609'.
               10  SCB-E-000894             OCCURS 4 TIMES.
                   15  SCB-EK-000894        PIC X(10).
                   15  SCB-EV-000894        PIC 9(06) COMP.
               10  SCB-A-000895             PIC 9(09) COMP VALUE 895.
               10  SCB-B-000896             PIC S9(07)V99 COMP-3 VALUE +0000896.
               10  SCB-C-000897             PIC X(24) VALUE 'FIELD-000897-ALPHA'.
               10  SCB-D-000898             PIC 9(04) VALUE 6286.
               10  SCB-DESC-000898          PIC X(18) VALUE 'DESC-011674'.
               10  SCB-E-000899             OCCURS 5 TIMES.
                   15  SCB-EK-000899        PIC X(10).
                   15  SCB-EV-000899        PIC 9(06) COMP.
               10  SCB-A-000900             PIC 9(09) COMP VALUE 900.
               10  SCB-B-000901             PIC S9(07)V99 COMP-3 VALUE +0000901.
               10  SCB-C-000902             PIC X(24) VALUE 'FIELD-000902-ALPHA'.
           05  SCB-GRP-0083.
               10  SCB-D-000903             PIC 9(04) VALUE 6321.
               10  SCB-DESC-000903          PIC X(18) VALUE 'DESC-011739'.
               10  SCB-E-000904             OCCURS 2 TIMES.
                   15  SCB-EK-000904        PIC X(10).
                   15  SCB-EV-000904        PIC 9(06) COMP.
               10  SCB-A-000905             PIC 9(09) COMP VALUE 905.
               10  SCB-B-000906             PIC S9(07)V99 COMP-3 VALUE +0000906.
               10  SCB-C-000907             PIC X(24) VALUE 'FIELD-000907-ALPHA'.
               10  SCB-D-000908             PIC 9(04) VALUE 6356.
               10  SCB-DESC-000908          PIC X(18) VALUE 'DESC-011804'.
               10  SCB-E-000909             OCCURS 3 TIMES.
                   15  SCB-EK-000909        PIC X(10).
                   15  SCB-EV-000909        PIC 9(06) COMP.
               10  SCB-A-000910             PIC 9(09) COMP VALUE 910.
               10  SCB-B-000911             PIC S9(07)V99 COMP-3 VALUE +0000911.
               10  SCB-C-000912             PIC X(24) VALUE 'FIELD-000912-ALPHA'.
               10  SCB-D-000913             PIC 9(04) VALUE 6391.
               10  SCB-DESC-000913          PIC X(18) VALUE 'DESC-011869'.
               10  SCB-E-000914             OCCURS 4 TIMES.
                   15  SCB-EK-000914        PIC X(10).
                   15  SCB-EV-000914        PIC 9(06) COMP.
               10  SCB-A-000915             PIC 9(09) COMP VALUE 915.
               10  SCB-B-000916             PIC S9(07)V99 COMP-3 VALUE +0000916.
           05  SCB-GRP-0084.
               10  SCB-C-000917             PIC X(24) VALUE 'FIELD-000917-ALPHA'.
               10  SCB-D-000918             PIC 9(04) VALUE 6426.
               10  SCB-DESC-000918          PIC X(18) VALUE 'DESC-011934'.
               10  SCB-E-000919             OCCURS 5 TIMES.
                   15  SCB-EK-000919        PIC X(10).
                   15  SCB-EV-000919        PIC 9(06) COMP.
               10  SCB-A-000920             PIC 9(09) COMP VALUE 920.
               10  SCB-B-000921             PIC S9(07)V99 COMP-3 VALUE +0000921.
               10  SCB-C-000922             PIC X(24) VALUE 'FIELD-000922-ALPHA'.
               10  SCB-D-000923             PIC 9(04) VALUE 6461.
               10  SCB-DESC-000923          PIC X(18) VALUE 'DESC-011999'.
               10  SCB-E-000924             OCCURS 2 TIMES.
                   15  SCB-EK-000924        PIC X(10).
                   15  SCB-EV-000924        PIC 9(06) COMP.
           05  SCB-ALT-0014 REDEFINES SCB-GRP-0084.
               10  SCB-ALT-FLAG-0014      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0014   PIC X(64).
           05  SCB-GRP-0085.
               10  SCB-A-000925             PIC 9(09) COMP VALUE 925.
               10  SCB-B-000926             PIC S9(07)V99 COMP-3 VALUE +0000926.
               10  SCB-C-000927             PIC X(24) VALUE 'FIELD-000927-ALPHA'.
               10  SCB-D-000928             PIC 9(04) VALUE 6496.
               10  SCB-DESC-000928          PIC X(18) VALUE 'DESC-012064'.
               10  SCB-E-000929             OCCURS 3 TIMES.
                   15  SCB-EK-000929        PIC X(10).
                   15  SCB-EV-000929        PIC 9(06) COMP.
               10  SCB-A-000930             PIC 9(09) COMP VALUE 930.
               10  SCB-B-000931             PIC S9(07)V99 COMP-3 VALUE +0000931.
               10  SCB-C-000932             PIC X(24) VALUE 'FIELD-000932-ALPHA'.
               10  SCB-D-000933             PIC 9(04) VALUE 6531.
               10  SCB-DESC-000933          PIC X(18) VALUE 'DESC-012129'.
           05  SCB-GRP-0086.
               10  SCB-E-000934             OCCURS 4 TIMES.
                   15  SCB-EK-000934        PIC X(10).
                   15  SCB-EV-000934        PIC 9(06) COMP.
               10  SCB-A-000935             PIC 9(09) COMP VALUE 935.
               10  SCB-B-000936             PIC S9(07)V99 COMP-3 VALUE +0000936.
               10  SCB-C-000937             PIC X(24) VALUE 'FIELD-000937-ALPHA'.
               10  SCB-D-000938             PIC 9(04) VALUE 6566.
               10  SCB-DESC-000938          PIC X(18) VALUE 'DESC-012194'.
               10  SCB-E-000939             OCCURS 5 TIMES.
                   15  SCB-EK-000939        PIC X(10).
                   15  SCB-EV-000939        PIC 9(06) COMP.
               10  SCB-A-000940             PIC 9(09) COMP VALUE 940.
               10  SCB-B-000941             PIC S9(07)V99 COMP-3 VALUE +0000941.
               10  SCB-C-000942             PIC X(24) VALUE 'FIELD-000942-ALPHA'.
               10  SCB-D-000943             PIC 9(04) VALUE 6601.
               10  SCB-DESC-000943          PIC X(18) VALUE 'DESC-012259'.
           05  SCB-GRP-0087.
               10  SCB-E-000944             OCCURS 2 TIMES.
                   15  SCB-EK-000944        PIC X(10).
                   15  SCB-EV-000944        PIC 9(06) COMP.
               10  SCB-A-000945             PIC 9(09) COMP VALUE 945.
               10  SCB-B-000946             PIC S9(07)V99 COMP-3 VALUE +0000946.
               10  SCB-C-000947             PIC X(24) VALUE 'FIELD-000947-ALPHA'.
               10  SCB-D-000948             PIC 9(04) VALUE 6636.
               10  SCB-DESC-000948          PIC X(18) VALUE 'DESC-012324'.
               10  SCB-E-000949             OCCURS 3 TIMES.
                   15  SCB-EK-000949        PIC X(10).
                   15  SCB-EV-000949        PIC 9(06) COMP.
               10  SCB-A-000950             PIC 9(09) COMP VALUE 950.
               10  SCB-B-000951             PIC S9(07)V99 COMP-3 VALUE +0000951.
               10  SCB-C-000952             PIC X(24) VALUE 'FIELD-000952-ALPHA'.
               10  SCB-D-000953             PIC 9(04) VALUE 6671.
               10  SCB-DESC-000953          PIC X(18) VALUE 'DESC-012389'.
               10  SCB-E-000954             OCCURS 4 TIMES.
                   15  SCB-EK-000954        PIC X(10).
                   15  SCB-EV-000954        PIC 9(06) COMP.
           05  SCB-GRP-0088.
               10  SCB-A-000955             PIC 9(09) COMP VALUE 955.
               10  SCB-B-000956             PIC S9(07)V99 COMP-3 VALUE +0000956.
               10  SCB-C-000957             PIC X(24) VALUE 'FIELD-000957-ALPHA'.
               10  SCB-D-000958             PIC 9(04) VALUE 6706.
               10  SCB-DESC-000958          PIC X(18) VALUE 'DESC-012454'.
               10  SCB-E-000959             OCCURS 5 TIMES.
                   15  SCB-EK-000959        PIC X(10).
                   15  SCB-EV-000959        PIC 9(06) COMP.
               10  SCB-A-000960             PIC 9(09) COMP VALUE 960.
               10  SCB-B-000961             PIC S9(07)V99 COMP-3 VALUE +0000961.
               10  SCB-C-000962             PIC X(24) VALUE 'FIELD-000962-ALPHA'.
               10  SCB-D-000963             PIC 9(04) VALUE 6741.
               10  SCB-DESC-000963          PIC X(18) VALUE 'DESC-012519'.
               10  SCB-E-000964             OCCURS 2 TIMES.
                   15  SCB-EK-000964        PIC X(10).
                   15  SCB-EV-000964        PIC 9(06) COMP.
               10  SCB-A-000965             PIC 9(09) COMP VALUE 965.
               10  SCB-B-000966             PIC S9(07)V99 COMP-3 VALUE +0000966.
           05  SCB-GRP-0089.
               10  SCB-C-000967             PIC X(24) VALUE 'FIELD-000967-ALPHA'.
               10  SCB-D-000968             PIC 9(04) VALUE 6776.
               10  SCB-DESC-000968          PIC X(18) VALUE 'DESC-012584'.
               10  SCB-E-000969             OCCURS 3 TIMES.
                   15  SCB-EK-000969        PIC X(10).
                   15  SCB-EV-000969        PIC 9(06) COMP.
               10  SCB-A-000970             PIC 9(09) COMP VALUE 970.
               10  SCB-B-000971             PIC S9(07)V99 COMP-3 VALUE +0000971.
               10  SCB-C-000972             PIC X(24) VALUE 'FIELD-000972-ALPHA'.
               10  SCB-D-000973             PIC 9(04) VALUE 6811.
               10  SCB-DESC-000973          PIC X(18) VALUE 'DESC-012649'.
               10  SCB-E-000974             OCCURS 4 TIMES.
                   15  SCB-EK-000974        PIC X(10).
                   15  SCB-EV-000974        PIC 9(06) COMP.
               10  SCB-A-000975             PIC 9(09) COMP VALUE 975.
               10  SCB-B-000976             PIC S9(07)V99 COMP-3 VALUE +0000976.
               10  SCB-C-000977             PIC X(24) VALUE 'FIELD-000977-ALPHA'.
               10  SCB-D-000978             PIC 9(04) VALUE 6846.
               10  SCB-DESC-000978          PIC X(18) VALUE 'DESC-012714'.
               10  SCB-E-000979             OCCURS 5 TIMES.
                   15  SCB-EK-000979        PIC X(10).
                   15  SCB-EV-000979        PIC 9(06) COMP.
           05  SCB-GRP-0090.
               10  SCB-A-000980             PIC 9(09) COMP VALUE 980.
               10  SCB-B-000981             PIC S9(07)V99 COMP-3 VALUE +0000981.
               10  SCB-C-000982             PIC X(24) VALUE 'FIELD-000982-ALPHA'.
               10  SCB-D-000983             PIC 9(04) VALUE 6881.
               10  SCB-DESC-000983          PIC X(18) VALUE 'DESC-012779'.
               10  SCB-E-000984             OCCURS 2 TIMES.
                   15  SCB-EK-000984        PIC X(10).
                   15  SCB-EV-000984        PIC 9(06) COMP.
               10  SCB-A-000985             PIC 9(09) COMP VALUE 985.
               10  SCB-B-000986             PIC S9(07)V99 COMP-3 VALUE +0000986.
               10  SCB-C-000987             PIC X(24) VALUE 'FIELD-000987-ALPHA'.
               10  SCB-D-000988             PIC 9(04) VALUE 6916.
               10  SCB-DESC-000988          PIC X(18) VALUE 'DESC-012844'.
               10  SCB-E-000989             OCCURS 3 TIMES.
                   15  SCB-EK-000989        PIC X(10).
                   15  SCB-EV-000989        PIC 9(06) COMP.
               10  SCB-A-000990             PIC 9(09) COMP VALUE 990.
               10  SCB-B-000991             PIC S9(07)V99 COMP-3 VALUE +0000991.
               10  SCB-C-000992             PIC X(24) VALUE 'FIELD-000992-ALPHA'.
               10  SCB-D-000993             PIC 9(04) VALUE 6951.
               10  SCB-DESC-000993          PIC X(18) VALUE 'DESC-012909'.
           05  SCB-ALT-0015 REDEFINES SCB-GRP-0090.
               10  SCB-ALT-FLAG-0015      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0015   PIC X(64).
           05  SCB-GRP-0091.
               10  SCB-E-000994             OCCURS 4 TIMES.
                   15  SCB-EK-000994        PIC X(10).
                   15  SCB-EV-000994        PIC 9(06) COMP.
               10  SCB-A-000995             PIC 9(09) COMP VALUE 995.
               10  SCB-B-000996             PIC S9(07)V99 COMP-3 VALUE +0000996.
               10  SCB-C-000997             PIC X(24) VALUE 'FIELD-000997-ALPHA'.
               10  SCB-D-000998             PIC 9(04) VALUE 6986.
               10  SCB-DESC-000998          PIC X(18) VALUE 'DESC-012974'.
               10  SCB-E-000999             OCCURS 5 TIMES.
                   15  SCB-EK-000999        PIC X(10).
                   15  SCB-EV-000999        PIC 9(06) COMP.
               10  SCB-A-001000             PIC 9(09) COMP VALUE 1000.
               10  SCB-B-001001             PIC S9(07)V99 COMP-3 VALUE +0001001.
           05  SCB-GRP-0092.
               10  SCB-C-001002             PIC X(24) VALUE 'FIELD-001002-ALPHA'.
               10  SCB-D-001003             PIC 9(04) VALUE 7021.
               10  SCB-DESC-001003          PIC X(18) VALUE 'DESC-013039'.
               10  SCB-E-001004             OCCURS 2 TIMES.
                   15  SCB-EK-001004        PIC X(10).
                   15  SCB-EV-001004        PIC 9(06) COMP.
               10  SCB-A-001005             PIC 9(09) COMP VALUE 1005.
               10  SCB-B-001006             PIC S9(07)V99 COMP-3 VALUE +0001006.
               10  SCB-C-001007             PIC X(24) VALUE 'FIELD-001007-ALPHA'.
               10  SCB-D-001008             PIC 9(04) VALUE 7056.
               10  SCB-DESC-001008          PIC X(18) VALUE 'DESC-013104'.
               10  SCB-E-001009             OCCURS 3 TIMES.
                   15  SCB-EK-001009        PIC X(10).
                   15  SCB-EV-001009        PIC 9(06) COMP.
               10  SCB-A-001010             PIC 9(09) COMP VALUE 1010.
           05  SCB-GRP-0093.
               10  SCB-B-001011             PIC S9(07)V99 COMP-3 VALUE +0001011.
               10  SCB-C-001012             PIC X(24) VALUE 'FIELD-001012-ALPHA'.
               10  SCB-D-001013             PIC 9(04) VALUE 7091.
               10  SCB-DESC-001013          PIC X(18) VALUE 'DESC-013169'.
               10  SCB-E-001014             OCCURS 4 TIMES.
                   15  SCB-EK-001014        PIC X(10).
                   15  SCB-EV-001014        PIC 9(06) COMP.
               10  SCB-A-001015             PIC 9(09) COMP VALUE 1015.
               10  SCB-B-001016             PIC S9(07)V99 COMP-3 VALUE +0001016.
               10  SCB-C-001017             PIC X(24) VALUE 'FIELD-001017-ALPHA'.
               10  SCB-D-001018             PIC 9(04) VALUE 7126.
               10  SCB-DESC-001018          PIC X(18) VALUE 'DESC-013234'.
               10  SCB-E-001019             OCCURS 5 TIMES.
                   15  SCB-EK-001019        PIC X(10).
                   15  SCB-EV-001019        PIC 9(06) COMP.
               10  SCB-A-001020             PIC 9(09) COMP VALUE 1020.
           05  SCB-GRP-0094.
               10  SCB-B-001021             PIC S9(07)V99 COMP-3 VALUE +0001021.
               10  SCB-C-001022             PIC X(24) VALUE 'FIELD-001022-ALPHA'.
               10  SCB-D-001023             PIC 9(04) VALUE 7161.
               10  SCB-DESC-001023          PIC X(18) VALUE 'DESC-013299'.
               10  SCB-E-001024             OCCURS 2 TIMES.
                   15  SCB-EK-001024        PIC X(10).
                   15  SCB-EV-001024        PIC 9(06) COMP.
               10  SCB-A-001025             PIC 9(09) COMP VALUE 1025.
               10  SCB-B-001026             PIC S9(07)V99 COMP-3 VALUE +0001026.
               10  SCB-C-001027             PIC X(24) VALUE 'FIELD-001027-ALPHA'.
               10  SCB-D-001028             PIC 9(04) VALUE 7196.
               10  SCB-DESC-001028          PIC X(18) VALUE 'DESC-013364'.
               10  SCB-E-001029             OCCURS 3 TIMES.
                   15  SCB-EK-001029        PIC X(10).
                   15  SCB-EV-001029        PIC 9(06) COMP.
               10  SCB-A-001030             PIC 9(09) COMP VALUE 1030.
               10  SCB-B-001031             PIC S9(07)V99 COMP-3 VALUE +0001031.
           05  SCB-GRP-0095.
               10  SCB-C-001032             PIC X(24) VALUE 'FIELD-001032-ALPHA'.
               10  SCB-D-001033             PIC 9(04) VALUE 7231.
               10  SCB-DESC-001033          PIC X(18) VALUE 'DESC-013429'.
               10  SCB-E-001034             OCCURS 4 TIMES.
                   15  SCB-EK-001034        PIC X(10).
                   15  SCB-EV-001034        PIC 9(06) COMP.
               10  SCB-A-001035             PIC 9(09) COMP VALUE 1035.
               10  SCB-B-001036             PIC S9(07)V99 COMP-3 VALUE +0001036.
               10  SCB-C-001037             PIC X(24) VALUE 'FIELD-001037-ALPHA'.
               10  SCB-D-001038             PIC 9(04) VALUE 7266.
               10  SCB-DESC-001038          PIC X(18) VALUE 'DESC-013494'.
               10  SCB-E-001039             OCCURS 5 TIMES.
                   15  SCB-EK-001039        PIC X(10).
                   15  SCB-EV-001039        PIC 9(06) COMP.
               10  SCB-A-001040             PIC 9(09) COMP VALUE 1040.
               10  SCB-B-001041             PIC S9(07)V99 COMP-3 VALUE +0001041.
               10  SCB-C-001042             PIC X(24) VALUE 'FIELD-001042-ALPHA'.
               10  SCB-D-001043             PIC 9(04) VALUE 7301.
               10  SCB-DESC-001043          PIC X(18) VALUE 'DESC-013559'.
           05  SCB-GRP-0096.
               10  SCB-E-001044             OCCURS 2 TIMES.
                   15  SCB-EK-001044        PIC X(10).
                   15  SCB-EV-001044        PIC 9(06) COMP.
               10  SCB-A-001045             PIC 9(09) COMP VALUE 1045.
               10  SCB-B-001046             PIC S9(07)V99 COMP-3 VALUE +0001046.
               10  SCB-C-001047             PIC X(24) VALUE 'FIELD-001047-ALPHA'.
               10  SCB-D-001048             PIC 9(04) VALUE 7336.
               10  SCB-DESC-001048          PIC X(18) VALUE 'DESC-013624'.
               10  SCB-E-001049             OCCURS 3 TIMES.
                   15  SCB-EK-001049        PIC X(10).
                   15  SCB-EV-001049        PIC 9(06) COMP.
               10  SCB-A-001050             PIC 9(09) COMP VALUE 1050.
               10  SCB-B-001051             PIC S9(07)V99 COMP-3 VALUE +0001051.
               10  SCB-C-001052             PIC X(24) VALUE 'FIELD-001052-ALPHA'.
               10  SCB-D-001053             PIC 9(04) VALUE 7371.
               10  SCB-DESC-001053          PIC X(18) VALUE 'DESC-013689'.
               10  SCB-E-001054             OCCURS 4 TIMES.
                   15  SCB-EK-001054        PIC X(10).
                   15  SCB-EV-001054        PIC 9(06) COMP.
               10  SCB-A-001055             PIC 9(09) COMP VALUE 1055.
               10  SCB-B-001056             PIC S9(07)V99 COMP-3 VALUE +0001056.
           05  SCB-ALT-0016 REDEFINES SCB-GRP-0096.
               10  SCB-ALT-FLAG-0016      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0016   PIC X(64).
           05  SCB-GRP-0097.
               10  SCB-C-001057             PIC X(24) VALUE 'FIELD-001057-ALPHA'.
               10  SCB-D-001058             PIC 9(04) VALUE 7406.
               10  SCB-DESC-001058          PIC X(18) VALUE 'DESC-013754'.
               10  SCB-E-001059             OCCURS 5 TIMES.
                   15  SCB-EK-001059        PIC X(10).
                   15  SCB-EV-001059        PIC 9(06) COMP.
               10  SCB-A-001060             PIC 9(09) COMP VALUE 1060.
               10  SCB-B-001061             PIC S9(07)V99 COMP-3 VALUE +0001061.
               10  SCB-C-001062             PIC X(24) VALUE 'FIELD-001062-ALPHA'.
               10  SCB-D-001063             PIC 9(04) VALUE 7441.
               10  SCB-DESC-001063          PIC X(18) VALUE 'DESC-013819'.
               10  SCB-E-001064             OCCURS 2 TIMES.
                   15  SCB-EK-001064        PIC X(10).
                   15  SCB-EV-001064        PIC 9(06) COMP.
               10  SCB-A-001065             PIC 9(09) COMP VALUE 1065.
               10  SCB-B-001066             PIC S9(07)V99 COMP-3 VALUE +0001066.
               10  SCB-C-001067             PIC X(24) VALUE 'FIELD-001067-ALPHA'.
               10  SCB-D-001068             PIC 9(04) VALUE 7476.
               10  SCB-DESC-001068          PIC X(18) VALUE 'DESC-013884'.
               10  SCB-E-001069             OCCURS 3 TIMES.
                   15  SCB-EK-001069        PIC X(10).
                   15  SCB-EV-001069        PIC 9(06) COMP.
               10  SCB-A-001070             PIC 9(09) COMP VALUE 1070.
           05  SCB-GRP-0098.
               10  SCB-B-001071             PIC S9(07)V99 COMP-3 VALUE +0001071.
               10  SCB-C-001072             PIC X(24) VALUE 'FIELD-001072-ALPHA'.
               10  SCB-D-001073             PIC 9(04) VALUE 7511.
               10  SCB-DESC-001073          PIC X(18) VALUE 'DESC-013949'.
               10  SCB-E-001074             OCCURS 4 TIMES.
                   15  SCB-EK-001074        PIC X(10).
                   15  SCB-EV-001074        PIC 9(06) COMP.
               10  SCB-A-001075             PIC 9(09) COMP VALUE 1075.
               10  SCB-B-001076             PIC S9(07)V99 COMP-3 VALUE +0001076.
               10  SCB-C-001077             PIC X(24) VALUE 'FIELD-001077-ALPHA'.
               10  SCB-D-001078             PIC 9(04) VALUE 7546.
               10  SCB-DESC-001078          PIC X(18) VALUE 'DESC-014014'.
           05  SCB-GRP-0099.
               10  SCB-E-001079             OCCURS 5 TIMES.
                   15  SCB-EK-001079        PIC X(10).
                   15  SCB-EV-001079        PIC 9(06) COMP.
               10  SCB-A-001080             PIC 9(09) COMP VALUE 1080.
               10  SCB-B-001081             PIC S9(07)V99 COMP-3 VALUE +0001081.
               10  SCB-C-001082             PIC X(24) VALUE 'FIELD-001082-ALPHA'.
               10  SCB-D-001083             PIC 9(04) VALUE 7581.
               10  SCB-DESC-001083          PIC X(18) VALUE 'DESC-014079'.
               10  SCB-E-001084             OCCURS 2 TIMES.
                   15  SCB-EK-001084        PIC X(10).
                   15  SCB-EV-001084        PIC 9(06) COMP.
               10  SCB-A-001085             PIC 9(09) COMP VALUE 1085.
               10  SCB-B-001086             PIC S9(07)V99 COMP-3 VALUE +0001086.
               10  SCB-C-001087             PIC X(24) VALUE 'FIELD-001087-ALPHA'.
           05  SCB-GRP-0100.
               10  SCB-D-001088             PIC 9(04) VALUE 7616.
               10  SCB-DESC-001088          PIC X(18) VALUE 'DESC-014144'.
               10  SCB-E-001089             OCCURS 3 TIMES.
                   15  SCB-EK-001089        PIC X(10).
                   15  SCB-EV-001089        PIC 9(06) COMP.
               10  SCB-A-001090             PIC 9(09) COMP VALUE 1090.
               10  SCB-B-001091             PIC S9(07)V99 COMP-3 VALUE +0001091.
               10  SCB-C-001092             PIC X(24) VALUE 'FIELD-001092-ALPHA'.
               10  SCB-D-001093             PIC 9(04) VALUE 7651.
               10  SCB-DESC-001093          PIC X(18) VALUE 'DESC-014209'.
               10  SCB-E-001094             OCCURS 4 TIMES.
                   15  SCB-EK-001094        PIC X(10).
                   15  SCB-EV-001094        PIC 9(06) COMP.
               10  SCB-A-001095             PIC 9(09) COMP VALUE 1095.
               10  SCB-B-001096             PIC S9(07)V99 COMP-3 VALUE +0001096.
               10  SCB-C-001097             PIC X(24) VALUE 'FIELD-001097-ALPHA'.
           05  SCB-GRP-0101.
               10  SCB-D-001098             PIC 9(04) VALUE 7686.
               10  SCB-DESC-001098          PIC X(18) VALUE 'DESC-014274'.
               10  SCB-E-001099             OCCURS 5 TIMES.
                   15  SCB-EK-001099        PIC X(10).
                   15  SCB-EV-001099        PIC 9(06) COMP.
               10  SCB-A-001100             PIC 9(09) COMP VALUE 1100.
               10  SCB-B-001101             PIC S9(07)V99 COMP-3 VALUE +0001101.
               10  SCB-C-001102             PIC X(24) VALUE 'FIELD-001102-ALPHA'.
               10  SCB-D-001103             PIC 9(04) VALUE 7721.
               10  SCB-DESC-001103          PIC X(18) VALUE 'DESC-014339'.
               10  SCB-E-001104             OCCURS 2 TIMES.
                   15  SCB-EK-001104        PIC X(10).
                   15  SCB-EV-001104        PIC 9(06) COMP.
               10  SCB-A-001105             PIC 9(09) COMP VALUE 1105.
               10  SCB-B-001106             PIC S9(07)V99 COMP-3 VALUE +0001106.
               10  SCB-C-001107             PIC X(24) VALUE 'FIELD-001107-ALPHA'.
               10  SCB-D-001108             PIC 9(04) VALUE 7756.
               10  SCB-DESC-001108          PIC X(18) VALUE 'DESC-014404'.
           05  SCB-GRP-0102.
               10  SCB-E-001109             OCCURS 3 TIMES.
                   15  SCB-EK-001109        PIC X(10).
                   15  SCB-EV-001109        PIC 9(06) COMP.
               10  SCB-A-001110             PIC 9(09) COMP VALUE 1110.
               10  SCB-B-001111             PIC S9(07)V99 COMP-3 VALUE +0001111.
               10  SCB-C-001112             PIC X(24) VALUE 'FIELD-001112-ALPHA'.
               10  SCB-D-001113             PIC 9(04) VALUE 7791.
               10  SCB-DESC-001113          PIC X(18) VALUE 'DESC-014469'.
               10  SCB-E-001114             OCCURS 4 TIMES.
                   15  SCB-EK-001114        PIC X(10).
                   15  SCB-EV-001114        PIC 9(06) COMP.
               10  SCB-A-001115             PIC 9(09) COMP VALUE 1115.
               10  SCB-B-001116             PIC S9(07)V99 COMP-3 VALUE +0001116.
               10  SCB-C-001117             PIC X(24) VALUE 'FIELD-001117-ALPHA'.
               10  SCB-D-001118             PIC 9(04) VALUE 7826.
               10  SCB-DESC-001118          PIC X(18) VALUE 'DESC-014534'.
               10  SCB-E-001119             OCCURS 5 TIMES.
                   15  SCB-EK-001119        PIC X(10).
                   15  SCB-EV-001119        PIC 9(06) COMP.
               10  SCB-A-001120             PIC 9(09) COMP VALUE 1120.
           05  SCB-ALT-0017 REDEFINES SCB-GRP-0102.
               10  SCB-ALT-FLAG-0017      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0017   PIC X(64).
           05  SCB-GRP-0103.
               10  SCB-B-001121             PIC S9(07)V99 COMP-3 VALUE +0001121.
               10  SCB-C-001122             PIC X(24) VALUE 'FIELD-001122-ALPHA'.
               10  SCB-D-001123             PIC 9(04) VALUE 7861.
               10  SCB-DESC-001123          PIC X(18) VALUE 'DESC-014599'.
               10  SCB-E-001124             OCCURS 2 TIMES.
                   15  SCB-EK-001124        PIC X(10).
                   15  SCB-EV-001124        PIC 9(06) COMP.
               10  SCB-A-001125             PIC 9(09) COMP VALUE 1125.
               10  SCB-B-001126             PIC S9(07)V99 COMP-3 VALUE +0001126.
               10  SCB-C-001127             PIC X(24) VALUE 'FIELD-001127-ALPHA'.
               10  SCB-D-001128             PIC 9(04) VALUE 7896.
               10  SCB-DESC-001128          PIC X(18) VALUE 'DESC-014664'.
               10  SCB-E-001129             OCCURS 3 TIMES.
                   15  SCB-EK-001129        PIC X(10).
                   15  SCB-EV-001129        PIC 9(06) COMP.
               10  SCB-A-001130             PIC 9(09) COMP VALUE 1130.
               10  SCB-B-001131             PIC S9(07)V99 COMP-3 VALUE +0001131.
               10  SCB-C-001132             PIC X(24) VALUE 'FIELD-001132-ALPHA'.
               10  SCB-D-001133             PIC 9(04) VALUE 7931.
               10  SCB-DESC-001133          PIC X(18) VALUE 'DESC-014729'.
           05  SCB-GRP-0104.
               10  SCB-E-001134             OCCURS 4 TIMES.
                   15  SCB-EK-001134        PIC X(10).
                   15  SCB-EV-001134        PIC 9(06) COMP.
               10  SCB-A-001135             PIC 9(09) COMP VALUE 1135.
               10  SCB-B-001136             PIC S9(07)V99 COMP-3 VALUE +0001136.
               10  SCB-C-001137             PIC X(24) VALUE 'FIELD-001137-ALPHA'.
               10  SCB-D-001138             PIC 9(04) VALUE 7966.
               10  SCB-DESC-001138          PIC X(18) VALUE 'DESC-014794'.
               10  SCB-E-001139             OCCURS 5 TIMES.
                   15  SCB-EK-001139        PIC X(10).
                   15  SCB-EV-001139        PIC 9(06) COMP.
               10  SCB-A-001140             PIC 9(09) COMP VALUE 1140.
               10  SCB-B-001141             PIC S9(07)V99 COMP-3 VALUE +0001141.
               10  SCB-C-001142             PIC X(24) VALUE 'FIELD-001142-ALPHA'.
               10  SCB-D-001143             PIC 9(04) VALUE 8001.
               10  SCB-DESC-001143          PIC X(18) VALUE 'DESC-014859'.
               10  SCB-E-001144             OCCURS 2 TIMES.
                   15  SCB-EK-001144        PIC X(10).
                   15  SCB-EV-001144        PIC 9(06) COMP.
               10  SCB-A-001145             PIC 9(09) COMP VALUE 1145.
               10  SCB-B-001146             PIC S9(07)V99 COMP-3 VALUE +0001146.
               10  SCB-C-001147             PIC X(24) VALUE 'FIELD-001147-ALPHA'.
           05  SCB-GRP-0105.
               10  SCB-D-001148             PIC 9(04) VALUE 8036.
               10  SCB-DESC-001148          PIC X(18) VALUE 'DESC-014924'.
               10  SCB-E-001149             OCCURS 3 TIMES.
                   15  SCB-EK-001149        PIC X(10).
                   15  SCB-EV-001149        PIC 9(06) COMP.
               10  SCB-A-001150             PIC 9(09) COMP VALUE 1150.
               10  SCB-B-001151             PIC S9(07)V99 COMP-3 VALUE +0001151.
               10  SCB-C-001152             PIC X(24) VALUE 'FIELD-001152-ALPHA'.
               10  SCB-D-001153             PIC 9(04) VALUE 8071.
               10  SCB-DESC-001153          PIC X(18) VALUE 'DESC-014989'.
               10  SCB-E-001154             OCCURS 4 TIMES.
                   15  SCB-EK-001154        PIC X(10).
                   15  SCB-EV-001154        PIC 9(06) COMP.
               10  SCB-A-001155             PIC 9(09) COMP VALUE 1155.
           05  SCB-GRP-0106.
               10  SCB-B-001156             PIC S9(07)V99 COMP-3 VALUE +0001156.
               10  SCB-C-001157             PIC X(24) VALUE 'FIELD-001157-ALPHA'.
               10  SCB-D-001158             PIC 9(04) VALUE 8106.
               10  SCB-DESC-001158          PIC X(18) VALUE 'DESC-015054'.
               10  SCB-E-001159             OCCURS 5 TIMES.
                   15  SCB-EK-001159        PIC X(10).
                   15  SCB-EV-001159        PIC 9(06) COMP.
               10  SCB-A-001160             PIC 9(09) COMP VALUE 1160.
               10  SCB-B-001161             PIC S9(07)V99 COMP-3 VALUE +0001161.
               10  SCB-C-001162             PIC X(24) VALUE 'FIELD-001162-ALPHA'.
               10  SCB-D-001163             PIC 9(04) VALUE 8141.
               10  SCB-DESC-001163          PIC X(18) VALUE 'DESC-015119'.
               10  SCB-E-001164             OCCURS 2 TIMES.
                   15  SCB-EK-001164        PIC X(10).
                   15  SCB-EV-001164        PIC 9(06) COMP.
           05  SCB-GRP-0107.
               10  SCB-A-001165             PIC 9(09) COMP VALUE 1165.
               10  SCB-B-001166             PIC S9(07)V99 COMP-3 VALUE +0001166.
               10  SCB-C-001167             PIC X(24) VALUE 'FIELD-001167-ALPHA'.
               10  SCB-D-001168             PIC 9(04) VALUE 8176.
               10  SCB-DESC-001168          PIC X(18) VALUE 'DESC-015184'.
               10  SCB-E-001169             OCCURS 3 TIMES.
                   15  SCB-EK-001169        PIC X(10).
                   15  SCB-EV-001169        PIC 9(06) COMP.
               10  SCB-A-001170             PIC 9(09) COMP VALUE 1170.
               10  SCB-B-001171             PIC S9(07)V99 COMP-3 VALUE +0001171.
               10  SCB-C-001172             PIC X(24) VALUE 'FIELD-001172-ALPHA'.
               10  SCB-D-001173             PIC 9(04) VALUE 8211.
               10  SCB-DESC-001173          PIC X(18) VALUE 'DESC-015249'.
               10  SCB-E-001174             OCCURS 4 TIMES.
                   15  SCB-EK-001174        PIC X(10).
                   15  SCB-EV-001174        PIC 9(06) COMP.
           05  SCB-GRP-0108.
               10  SCB-A-001175             PIC 9(09) COMP VALUE 1175.
               10  SCB-B-001176             PIC S9(07)V99 COMP-3 VALUE +0001176.
               10  SCB-C-001177             PIC X(24) VALUE 'FIELD-001177-ALPHA'.
               10  SCB-D-001178             PIC 9(04) VALUE 8246.
               10  SCB-DESC-001178          PIC X(18) VALUE 'DESC-015314'.
               10  SCB-E-001179             OCCURS 5 TIMES.
                   15  SCB-EK-001179        PIC X(10).
                   15  SCB-EV-001179        PIC 9(06) COMP.
               10  SCB-A-001180             PIC 9(09) COMP VALUE 1180.
               10  SCB-B-001181             PIC S9(07)V99 COMP-3 VALUE +0001181.
               10  SCB-C-001182             PIC X(24) VALUE 'FIELD-001182-ALPHA'.
               10  SCB-D-001183             PIC 9(04) VALUE 8281.
               10  SCB-DESC-001183          PIC X(18) VALUE 'DESC-015379'.
               10  SCB-E-001184             OCCURS 2 TIMES.
                   15  SCB-EK-001184        PIC X(10).
                   15  SCB-EV-001184        PIC 9(06) COMP.
               10  SCB-A-001185             PIC 9(09) COMP VALUE 1185.
           05  SCB-ALT-0018 REDEFINES SCB-GRP-0108.
               10  SCB-ALT-FLAG-0018      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0018   PIC X(64).
           05  SCB-GRP-0109.
               10  SCB-B-001186             PIC S9(07)V99 COMP-3 VALUE +0001186.
               10  SCB-C-001187             PIC X(24) VALUE 'FIELD-001187-ALPHA'.
               10  SCB-D-001188             PIC 9(04) VALUE 8316.
               10  SCB-DESC-001188          PIC X(18) VALUE 'DESC-015444'.
               10  SCB-E-001189             OCCURS 3 TIMES.
                   15  SCB-EK-001189        PIC X(10).
                   15  SCB-EV-001189        PIC 9(06) COMP.
               10  SCB-A-001190             PIC 9(09) COMP VALUE 1190.
               10  SCB-B-001191             PIC S9(07)V99 COMP-3 VALUE +0001191.
               10  SCB-C-001192             PIC X(24) VALUE 'FIELD-001192-ALPHA'.
               10  SCB-D-001193             PIC 9(04) VALUE 8351.
               10  SCB-DESC-001193          PIC X(18) VALUE 'DESC-015509'.
               10  SCB-E-001194             OCCURS 4 TIMES.
                   15  SCB-EK-001194        PIC X(10).
                   15  SCB-EV-001194        PIC 9(06) COMP.
               10  SCB-A-001195             PIC 9(09) COMP VALUE 1195.
               10  SCB-B-001196             PIC S9(07)V99 COMP-3 VALUE +0001196.
               10  SCB-C-001197             PIC X(24) VALUE 'FIELD-001197-ALPHA'.
           05  SCB-GRP-0110.
               10  SCB-D-001198             PIC 9(04) VALUE 8386.
               10  SCB-DESC-001198          PIC X(18) VALUE 'DESC-015574'.
               10  SCB-E-001199             OCCURS 5 TIMES.
                   15  SCB-EK-001199        PIC X(10).
                   15  SCB-EV-001199        PIC 9(06) COMP.
               10  SCB-A-001200             PIC 9(09) COMP VALUE 1200.
               10  SCB-B-001201             PIC S9(07)V99 COMP-3 VALUE +0001201.
               10  SCB-C-001202             PIC X(24) VALUE 'FIELD-001202-ALPHA'.
               10  SCB-D-001203             PIC 9(04) VALUE 8421.
               10  SCB-DESC-001203          PIC X(18) VALUE 'DESC-015639'.
               10  SCB-E-001204             OCCURS 2 TIMES.
                   15  SCB-EK-001204        PIC X(10).
                   15  SCB-EV-001204        PIC 9(06) COMP.
               10  SCB-A-001205             PIC 9(09) COMP VALUE 1205.
               10  SCB-B-001206             PIC S9(07)V99 COMP-3 VALUE +0001206.
               10  SCB-C-001207             PIC X(24) VALUE 'FIELD-001207-ALPHA'.
               10  SCB-D-001208             PIC 9(04) VALUE 8456.
               10  SCB-DESC-001208          PIC X(18) VALUE 'DESC-015704'.
               10  SCB-E-001209             OCCURS 3 TIMES.
                   15  SCB-EK-001209        PIC X(10).
                   15  SCB-EV-001209        PIC 9(06) COMP.
               10  SCB-A-001210             PIC 9(09) COMP VALUE 1210.
           05  SCB-GRP-0111.
               10  SCB-B-001211             PIC S9(07)V99 COMP-3 VALUE +0001211.
               10  SCB-C-001212             PIC X(24) VALUE 'FIELD-001212-ALPHA'.
               10  SCB-D-001213             PIC 9(04) VALUE 8491.
               10  SCB-DESC-001213          PIC X(18) VALUE 'DESC-015769'.
               10  SCB-E-001214             OCCURS 4 TIMES.
                   15  SCB-EK-001214        PIC X(10).
                   15  SCB-EV-001214        PIC 9(06) COMP.
               10  SCB-A-001215             PIC 9(09) COMP VALUE 1215.
               10  SCB-B-001216             PIC S9(07)V99 COMP-3 VALUE +0001216.
               10  SCB-C-001217             PIC X(24) VALUE 'FIELD-001217-ALPHA'.
               10  SCB-D-001218             PIC 9(04) VALUE 8526.
               10  SCB-DESC-001218          PIC X(18) VALUE 'DESC-015834'.
               10  SCB-E-001219             OCCURS 5 TIMES.
                   15  SCB-EK-001219        PIC X(10).
                   15  SCB-EV-001219        PIC 9(06) COMP.
               10  SCB-A-001220             PIC 9(09) COMP VALUE 1220.
               10  SCB-B-001221             PIC S9(07)V99 COMP-3 VALUE +0001221.
               10  SCB-C-001222             PIC X(24) VALUE 'FIELD-001222-ALPHA'.
               10  SCB-D-001223             PIC 9(04) VALUE 8561.
               10  SCB-DESC-001223          PIC X(18) VALUE 'DESC-015899'.
               10  SCB-E-001224             OCCURS 2 TIMES.
                   15  SCB-EK-001224        PIC X(10).
                   15  SCB-EV-001224        PIC 9(06) COMP.
           05  SCB-GRP-0112.
               10  SCB-A-001225             PIC 9(09) COMP VALUE 1225.
               10  SCB-B-001226             PIC S9(07)V99 COMP-3 VALUE +0001226.
               10  SCB-C-001227             PIC X(24) VALUE 'FIELD-001227-ALPHA'.
               10  SCB-D-001228             PIC 9(04) VALUE 8596.
               10  SCB-DESC-001228          PIC X(18) VALUE 'DESC-015964'.
               10  SCB-E-001229             OCCURS 3 TIMES.
                   15  SCB-EK-001229        PIC X(10).
                   15  SCB-EV-001229        PIC 9(06) COMP.
               10  SCB-A-001230             PIC 9(09) COMP VALUE 1230.
               10  SCB-B-001231             PIC S9(07)V99 COMP-3 VALUE +0001231.
               10  SCB-C-001232             PIC X(24) VALUE 'FIELD-001232-ALPHA'.
           05  SCB-GRP-0113.
               10  SCB-D-001233             PIC 9(04) VALUE 8631.
               10  SCB-DESC-001233          PIC X(18) VALUE 'DESC-016029'.
               10  SCB-E-001234             OCCURS 4 TIMES.
                   15  SCB-EK-001234        PIC X(10).
                   15  SCB-EV-001234        PIC 9(06) COMP.
               10  SCB-A-001235             PIC 9(09) COMP VALUE 1235.
               10  SCB-B-001236             PIC S9(07)V99 COMP-3 VALUE +0001236.
               10  SCB-C-001237             PIC X(24) VALUE 'FIELD-001237-ALPHA'.
               10  SCB-D-001238             PIC 9(04) VALUE 8666.
               10  SCB-DESC-001238          PIC X(18) VALUE 'DESC-016094'.
               10  SCB-E-001239             OCCURS 5 TIMES.
                   15  SCB-EK-001239        PIC X(10).
                   15  SCB-EV-001239        PIC 9(06) COMP.
               10  SCB-A-001240             PIC 9(09) COMP VALUE 1240.
               10  SCB-B-001241             PIC S9(07)V99 COMP-3 VALUE +0001241.
           05  SCB-GRP-0114.
               10  SCB-C-001242             PIC X(24) VALUE 'FIELD-001242-ALPHA'.
               10  SCB-D-001243             PIC 9(04) VALUE 8701.
               10  SCB-DESC-001243          PIC X(18) VALUE 'DESC-016159'.
               10  SCB-E-001244             OCCURS 2 TIMES.
                   15  SCB-EK-001244        PIC X(10).
                   15  SCB-EV-001244        PIC 9(06) COMP.
               10  SCB-A-001245             PIC 9(09) COMP VALUE 1245.
               10  SCB-B-001246             PIC S9(07)V99 COMP-3 VALUE +0001246.
               10  SCB-C-001247             PIC X(24) VALUE 'FIELD-001247-ALPHA'.
               10  SCB-D-001248             PIC 9(04) VALUE 8736.
               10  SCB-DESC-001248          PIC X(18) VALUE 'DESC-016224'.
               10  SCB-E-001249             OCCURS 3 TIMES.
                   15  SCB-EK-001249        PIC X(10).
                   15  SCB-EV-001249        PIC 9(06) COMP.
               10  SCB-A-001250             PIC 9(09) COMP VALUE 1250.
               10  SCB-B-001251             PIC S9(07)V99 COMP-3 VALUE +0001251.
           05  SCB-ALT-0019 REDEFINES SCB-GRP-0114.
               10  SCB-ALT-FLAG-0019      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0019   PIC X(64).
           05  SCB-GRP-0115.
               10  SCB-C-001252             PIC X(24) VALUE 'FIELD-001252-ALPHA'.
               10  SCB-D-001253             PIC 9(04) VALUE 8771.
               10  SCB-DESC-001253          PIC X(18) VALUE 'DESC-016289'.
               10  SCB-E-001254             OCCURS 4 TIMES.
                   15  SCB-EK-001254        PIC X(10).
                   15  SCB-EV-001254        PIC 9(06) COMP.
               10  SCB-A-001255             PIC 9(09) COMP VALUE 1255.
               10  SCB-B-001256             PIC S9(07)V99 COMP-3 VALUE +0001256.
               10  SCB-C-001257             PIC X(24) VALUE 'FIELD-001257-ALPHA'.
               10  SCB-D-001258             PIC 9(04) VALUE 8806.
               10  SCB-DESC-001258          PIC X(18) VALUE 'DESC-016354'.
               10  SCB-E-001259             OCCURS 5 TIMES.
                   15  SCB-EK-001259        PIC X(10).
                   15  SCB-EV-001259        PIC 9(06) COMP.
               10  SCB-A-001260             PIC 9(09) COMP VALUE 1260.
               10  SCB-B-001261             PIC S9(07)V99 COMP-3 VALUE +0001261.
               10  SCB-C-001262             PIC X(24) VALUE 'FIELD-001262-ALPHA'.
           05  SCB-GRP-0116.
               10  SCB-D-001263             PIC 9(04) VALUE 8841.
               10  SCB-DESC-001263          PIC X(18) VALUE 'DESC-016419'.
               10  SCB-E-001264             OCCURS 2 TIMES.
                   15  SCB-EK-001264        PIC X(10).
                   15  SCB-EV-001264        PIC 9(06) COMP.
               10  SCB-A-001265             PIC 9(09) COMP VALUE 1265.
               10  SCB-B-001266             PIC S9(07)V99 COMP-3 VALUE +0001266.
               10  SCB-C-001267             PIC X(24) VALUE 'FIELD-001267-ALPHA'.
               10  SCB-D-001268             PIC 9(04) VALUE 8876.
               10  SCB-DESC-001268          PIC X(18) VALUE 'DESC-016484'.
               10  SCB-E-001269             OCCURS 3 TIMES.
                   15  SCB-EK-001269        PIC X(10).
                   15  SCB-EV-001269        PIC 9(06) COMP.
               10  SCB-A-001270             PIC 9(09) COMP VALUE 1270.
               10  SCB-B-001271             PIC S9(07)V99 COMP-3 VALUE +0001271.
               10  SCB-C-001272             PIC X(24) VALUE 'FIELD-001272-ALPHA'.
               10  SCB-D-001273             PIC 9(04) VALUE 8911.
               10  SCB-DESC-001273          PIC X(18) VALUE 'DESC-016549'.
               10  SCB-E-001274             OCCURS 4 TIMES.
                   15  SCB-EK-001274        PIC X(10).
                   15  SCB-EV-001274        PIC 9(06) COMP.
           05  SCB-GRP-0117.
               10  SCB-A-001275             PIC 9(09) COMP VALUE 1275.
               10  SCB-B-001276             PIC S9(07)V99 COMP-3 VALUE +0001276.
               10  SCB-C-001277             PIC X(24) VALUE 'FIELD-001277-ALPHA'.
               10  SCB-D-001278             PIC 9(04) VALUE 8946.
               10  SCB-DESC-001278          PIC X(18) VALUE 'DESC-016614'.
               10  SCB-E-001279             OCCURS 5 TIMES.
                   15  SCB-EK-001279        PIC X(10).
                   15  SCB-EV-001279        PIC 9(06) COMP.
               10  SCB-A-001280             PIC 9(09) COMP VALUE 1280.
               10  SCB-B-001281             PIC S9(07)V99 COMP-3 VALUE +0001281.
               10  SCB-C-001282             PIC X(24) VALUE 'FIELD-001282-ALPHA'.
               10  SCB-D-001283             PIC 9(04) VALUE 8981.
               10  SCB-DESC-001283          PIC X(18) VALUE 'DESC-016679'.
               10  SCB-E-001284             OCCURS 2 TIMES.
                   15  SCB-EK-001284        PIC X(10).
                   15  SCB-EV-001284        PIC 9(06) COMP.
               10  SCB-A-001285             PIC 9(09) COMP VALUE 1285.
               10  SCB-B-001286             PIC S9(07)V99 COMP-3 VALUE +0001286.
               10  SCB-C-001287             PIC X(24) VALUE 'FIELD-001287-ALPHA'.
           05  SCB-GRP-0118.
               10  SCB-D-001288             PIC 9(04) VALUE 9016.
               10  SCB-DESC-001288          PIC X(18) VALUE 'DESC-016744'.
               10  SCB-E-001289             OCCURS 3 TIMES.
                   15  SCB-EK-001289        PIC X(10).
                   15  SCB-EV-001289        PIC 9(06) COMP.
               10  SCB-A-001290             PIC 9(09) COMP VALUE 1290.
               10  SCB-B-001291             PIC S9(07)V99 COMP-3 VALUE +0001291.
               10  SCB-C-001292             PIC X(24) VALUE 'FIELD-001292-ALPHA'.
               10  SCB-D-001293             PIC 9(04) VALUE 9051.
               10  SCB-DESC-001293          PIC X(18) VALUE 'DESC-016809'.
               10  SCB-E-001294             OCCURS 4 TIMES.
                   15  SCB-EK-001294        PIC X(10).
                   15  SCB-EV-001294        PIC 9(06) COMP.
               10  SCB-A-001295             PIC 9(09) COMP VALUE 1295.
               10  SCB-B-001296             PIC S9(07)V99 COMP-3 VALUE +0001296.
               10  SCB-C-001297             PIC X(24) VALUE 'FIELD-001297-ALPHA'.
               10  SCB-D-001298             PIC 9(04) VALUE 9086.
               10  SCB-DESC-001298          PIC X(18) VALUE 'DESC-016874'.
               10  SCB-E-001299             OCCURS 5 TIMES.
                   15  SCB-EK-001299        PIC X(10).
                   15  SCB-EV-001299        PIC 9(06) COMP.
               10  SCB-A-001300             PIC 9(09) COMP VALUE 1300.
               10  SCB-B-001301             PIC S9(07)V99 COMP-3 VALUE +0001301.
           05  SCB-GRP-0119.
               10  SCB-C-001302             PIC X(24) VALUE 'FIELD-001302-ALPHA'.
               10  SCB-D-001303             PIC 9(04) VALUE 9121.
               10  SCB-DESC-001303          PIC X(18) VALUE 'DESC-016939'.
               10  SCB-E-001304             OCCURS 2 TIMES.
                   15  SCB-EK-001304        PIC X(10).
                   15  SCB-EV-001304        PIC 9(06) COMP.
               10  SCB-A-001305             PIC 9(09) COMP VALUE 1305.
               10  SCB-B-001306             PIC S9(07)V99 COMP-3 VALUE +0001306.
               10  SCB-C-001307             PIC X(24) VALUE 'FIELD-001307-ALPHA'.
               10  SCB-D-001308             PIC 9(04) VALUE 9156.
               10  SCB-DESC-001308          PIC X(18) VALUE 'DESC-017004'.
               10  SCB-E-001309             OCCURS 3 TIMES.
                   15  SCB-EK-001309        PIC X(10).
                   15  SCB-EV-001309        PIC 9(06) COMP.
           05  SCB-GRP-0120.
               10  SCB-A-001310             PIC 9(09) COMP VALUE 1310.
               10  SCB-B-001311             PIC S9(07)V99 COMP-3 VALUE +0001311.
               10  SCB-C-001312             PIC X(24) VALUE 'FIELD-001312-ALPHA'.
               10  SCB-D-001313             PIC 9(04) VALUE 9191.
               10  SCB-DESC-001313          PIC X(18) VALUE 'DESC-017069'.
               10  SCB-E-001314             OCCURS 4 TIMES.
                   15  SCB-EK-001314        PIC X(10).
                   15  SCB-EV-001314        PIC 9(06) COMP.
               10  SCB-A-001315             PIC 9(09) COMP VALUE 1315.
               10  SCB-B-001316             PIC S9(07)V99 COMP-3 VALUE +0001316.
               10  SCB-C-001317             PIC X(24) VALUE 'FIELD-001317-ALPHA'.
               10  SCB-D-001318             PIC 9(04) VALUE 9226.
               10  SCB-DESC-001318          PIC X(18) VALUE 'DESC-017134'.
           05  SCB-ALT-0020 REDEFINES SCB-GRP-0120.
               10  SCB-ALT-FLAG-0020      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0020   PIC X(64).
           05  SCB-GRP-0121.
               10  SCB-E-001319             OCCURS 5 TIMES.
                   15  SCB-EK-001319        PIC X(10).
                   15  SCB-EV-001319        PIC 9(06) COMP.
               10  SCB-A-001320             PIC 9(09) COMP VALUE 1320.
               10  SCB-B-001321             PIC S9(07)V99 COMP-3 VALUE +0001321.
               10  SCB-C-001322             PIC X(24) VALUE 'FIELD-001322-ALPHA'.
               10  SCB-D-001323             PIC 9(04) VALUE 9261.
               10  SCB-DESC-001323          PIC X(18) VALUE 'DESC-017199'.
               10  SCB-E-001324             OCCURS 2 TIMES.
                   15  SCB-EK-001324        PIC X(10).
                   15  SCB-EV-001324        PIC 9(06) COMP.
               10  SCB-A-001325             PIC 9(09) COMP VALUE 1325.
               10  SCB-B-001326             PIC S9(07)V99 COMP-3 VALUE +0001326.
               10  SCB-C-001327             PIC X(24) VALUE 'FIELD-001327-ALPHA'.
               10  SCB-D-001328             PIC 9(04) VALUE 9296.
               10  SCB-DESC-001328          PIC X(18) VALUE 'DESC-017264'.
           05  SCB-GRP-0122.
               10  SCB-E-001329             OCCURS 3 TIMES.
                   15  SCB-EK-001329        PIC X(10).
                   15  SCB-EV-001329        PIC 9(06) COMP.
               10  SCB-A-001330             PIC 9(09) COMP VALUE 1330.
               10  SCB-B-001331             PIC S9(07)V99 COMP-3 VALUE +0001331.
               10  SCB-C-001332             PIC X(24) VALUE 'FIELD-001332-ALPHA'.
               10  SCB-D-001333             PIC 9(04) VALUE 9331.
               10  SCB-DESC-001333          PIC X(18) VALUE 'DESC-017329'.
               10  SCB-E-001334             OCCURS 4 TIMES.
                   15  SCB-EK-001334        PIC X(10).
                   15  SCB-EV-001334        PIC 9(06) COMP.
               10  SCB-A-001335             PIC 9(09) COMP VALUE 1335.
               10  SCB-B-001336             PIC S9(07)V99 COMP-3 VALUE +0001336.
               10  SCB-C-001337             PIC X(24) VALUE 'FIELD-001337-ALPHA'.
               10  SCB-D-001338             PIC 9(04) VALUE 9366.
               10  SCB-DESC-001338          PIC X(18) VALUE 'DESC-017394'.
               10  SCB-E-001339             OCCURS 5 TIMES.
                   15  SCB-EK-001339        PIC X(10).
                   15  SCB-EV-001339        PIC 9(06) COMP.
           05  SCB-GRP-0123.
               10  SCB-A-001340             PIC 9(09) COMP VALUE 1340.
               10  SCB-B-001341             PIC S9(07)V99 COMP-3 VALUE +0001341.
               10  SCB-C-001342             PIC X(24) VALUE 'FIELD-001342-ALPHA'.
               10  SCB-D-001343             PIC 9(04) VALUE 9401.
               10  SCB-DESC-001343          PIC X(18) VALUE 'DESC-017459'.
               10  SCB-E-001344             OCCURS 2 TIMES.
                   15  SCB-EK-001344        PIC X(10).
                   15  SCB-EV-001344        PIC 9(06) COMP.
               10  SCB-A-001345             PIC 9(09) COMP VALUE 1345.
               10  SCB-B-001346             PIC S9(07)V99 COMP-3 VALUE +0001346.
               10  SCB-C-001347             PIC X(24) VALUE 'FIELD-001347-ALPHA'.
               10  SCB-D-001348             PIC 9(04) VALUE 9436.
               10  SCB-DESC-001348          PIC X(18) VALUE 'DESC-017524'.
               10  SCB-E-001349             OCCURS 3 TIMES.
                   15  SCB-EK-001349        PIC X(10).
                   15  SCB-EV-001349        PIC 9(06) COMP.
               10  SCB-A-001350             PIC 9(09) COMP VALUE 1350.
               10  SCB-B-001351             PIC S9(07)V99 COMP-3 VALUE +0001351.
           05  SCB-GRP-0124.
               10  SCB-C-001352             PIC X(24) VALUE 'FIELD-001352-ALPHA'.
               10  SCB-D-001353             PIC 9(04) VALUE 9471.
               10  SCB-DESC-001353          PIC X(18) VALUE 'DESC-017589'.
               10  SCB-E-001354             OCCURS 4 TIMES.
                   15  SCB-EK-001354        PIC X(10).
                   15  SCB-EV-001354        PIC 9(06) COMP.
               10  SCB-A-001355             PIC 9(09) COMP VALUE 1355.
               10  SCB-B-001356             PIC S9(07)V99 COMP-3 VALUE +0001356.
               10  SCB-C-001357             PIC X(24) VALUE 'FIELD-001357-ALPHA'.
               10  SCB-D-001358             PIC 9(04) VALUE 9506.
               10  SCB-DESC-001358          PIC X(18) VALUE 'DESC-017654'.
               10  SCB-E-001359             OCCURS 5 TIMES.
                   15  SCB-EK-001359        PIC X(10).
                   15  SCB-EV-001359        PIC 9(06) COMP.
               10  SCB-A-001360             PIC 9(09) COMP VALUE 1360.
               10  SCB-B-001361             PIC S9(07)V99 COMP-3 VALUE +0001361.
               10  SCB-C-001362             PIC X(24) VALUE 'FIELD-001362-ALPHA'.
               10  SCB-D-001363             PIC 9(04) VALUE 9541.
               10  SCB-DESC-001363          PIC X(18) VALUE 'DESC-017719'.
               10  SCB-E-001364             OCCURS 2 TIMES.
                   15  SCB-EK-001364        PIC X(10).
                   15  SCB-EV-001364        PIC 9(06) COMP.
           05  SCB-GRP-0125.
               10  SCB-A-001365             PIC 9(09) COMP VALUE 1365.
               10  SCB-B-001366             PIC S9(07)V99 COMP-3 VALUE +0001366.
               10  SCB-C-001367             PIC X(24) VALUE 'FIELD-001367-ALPHA'.
               10  SCB-D-001368             PIC 9(04) VALUE 9576.
               10  SCB-DESC-001368          PIC X(18) VALUE 'DESC-017784'.
               10  SCB-E-001369             OCCURS 3 TIMES.
                   15  SCB-EK-001369        PIC X(10).
                   15  SCB-EV-001369        PIC 9(06) COMP.
               10  SCB-A-001370             PIC 9(09) COMP VALUE 1370.
               10  SCB-B-001371             PIC S9(07)V99 COMP-3 VALUE +0001371.
               10  SCB-C-001372             PIC X(24) VALUE 'FIELD-001372-ALPHA'.
               10  SCB-D-001373             PIC 9(04) VALUE 9611.
               10  SCB-DESC-001373          PIC X(18) VALUE 'DESC-017849'.
               10  SCB-E-001374             OCCURS 4 TIMES.
                   15  SCB-EK-001374        PIC X(10).
                   15  SCB-EV-001374        PIC 9(06) COMP.
               10  SCB-A-001375             PIC 9(09) COMP VALUE 1375.
               10  SCB-B-001376             PIC S9(07)V99 COMP-3 VALUE +0001376.
               10  SCB-C-001377             PIC X(24) VALUE 'FIELD-001377-ALPHA'.
               10  SCB-D-001378             PIC 9(04) VALUE 9646.
               10  SCB-DESC-001378          PIC X(18) VALUE 'DESC-017914'.
           05  SCB-GRP-0126.
               10  SCB-E-001379             OCCURS 5 TIMES.
                   15  SCB-EK-001379        PIC X(10).
                   15  SCB-EV-001379        PIC 9(06) COMP.
               10  SCB-A-001380             PIC 9(09) COMP VALUE 1380.
               10  SCB-B-001381             PIC S9(07)V99 COMP-3 VALUE +0001381.
               10  SCB-C-001382             PIC X(24) VALUE 'FIELD-001382-ALPHA'.
               10  SCB-D-001383             PIC 9(04) VALUE 9681.
               10  SCB-DESC-001383          PIC X(18) VALUE 'DESC-017979'.
               10  SCB-E-001384             OCCURS 2 TIMES.
                   15  SCB-EK-001384        PIC X(10).
                   15  SCB-EV-001384        PIC 9(06) COMP.
               10  SCB-A-001385             PIC 9(09) COMP VALUE 1385.
               10  SCB-B-001386             PIC S9(07)V99 COMP-3 VALUE +0001386.
           05  SCB-ALT-0021 REDEFINES SCB-GRP-0126.
               10  SCB-ALT-FLAG-0021      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0021   PIC X(64).
           05  SCB-GRP-0127.
               10  SCB-C-001387             PIC X(24) VALUE 'FIELD-001387-ALPHA'.
               10  SCB-D-001388             PIC 9(04) VALUE 9716.
               10  SCB-DESC-001388          PIC X(18) VALUE 'DESC-018044'.
               10  SCB-E-001389             OCCURS 3 TIMES.
                   15  SCB-EK-001389        PIC X(10).
                   15  SCB-EV-001389        PIC 9(06) COMP.
               10  SCB-A-001390             PIC 9(09) COMP VALUE 1390.
               10  SCB-B-001391             PIC S9(07)V99 COMP-3 VALUE +0001391.
               10  SCB-C-001392             PIC X(24) VALUE 'FIELD-001392-ALPHA'.
               10  SCB-D-001393             PIC 9(04) VALUE 9751.
               10  SCB-DESC-001393          PIC X(18) VALUE 'DESC-018109'.
               10  SCB-E-001394             OCCURS 4 TIMES.
                   15  SCB-EK-001394        PIC X(10).
                   15  SCB-EV-001394        PIC 9(06) COMP.
               10  SCB-A-001395             PIC 9(09) COMP VALUE 1395.
           05  SCB-GRP-0128.
               10  SCB-B-001396             PIC S9(07)V99 COMP-3 VALUE +0001396.
               10  SCB-C-001397             PIC X(24) VALUE 'FIELD-001397-ALPHA'.
               10  SCB-D-001398             PIC 9(04) VALUE 9786.
               10  SCB-DESC-001398          PIC X(18) VALUE 'DESC-018174'.
               10  SCB-E-001399             OCCURS 5 TIMES.
                   15  SCB-EK-001399        PIC X(10).
                   15  SCB-EV-001399        PIC 9(06) COMP.
               10  SCB-A-001400             PIC 9(09) COMP VALUE 1400.
               10  SCB-B-001401             PIC S9(07)V99 COMP-3 VALUE +0001401.
               10  SCB-C-001402             PIC X(24) VALUE 'FIELD-001402-ALPHA'.
               10  SCB-D-001403             PIC 9(04) VALUE 9821.
               10  SCB-DESC-001403          PIC X(18) VALUE 'DESC-018239'.
               10  SCB-E-001404             OCCURS 2 TIMES.
                   15  SCB-EK-001404        PIC X(10).
                   15  SCB-EV-001404        PIC 9(06) COMP.
               10  SCB-A-001405             PIC 9(09) COMP VALUE 1405.
           05  SCB-GRP-0129.
               10  SCB-B-001406             PIC S9(07)V99 COMP-3 VALUE +0001406.
               10  SCB-C-001407             PIC X(24) VALUE 'FIELD-001407-ALPHA'.
               10  SCB-D-001408             PIC 9(04) VALUE 9856.
               10  SCB-DESC-001408          PIC X(18) VALUE 'DESC-018304'.
               10  SCB-E-001409             OCCURS 3 TIMES.
                   15  SCB-EK-001409        PIC X(10).
                   15  SCB-EV-001409        PIC 9(06) COMP.
               10  SCB-A-001410             PIC 9(09) COMP VALUE 1410.
               10  SCB-B-001411             PIC S9(07)V99 COMP-3 VALUE +0001411.
               10  SCB-C-001412             PIC X(24) VALUE 'FIELD-001412-ALPHA'.
               10  SCB-D-001413             PIC 9(04) VALUE 9891.
               10  SCB-DESC-001413          PIC X(18) VALUE 'DESC-018369'.
               10  SCB-E-001414             OCCURS 4 TIMES.
                   15  SCB-EK-001414        PIC X(10).
                   15  SCB-EV-001414        PIC 9(06) COMP.
               10  SCB-A-001415             PIC 9(09) COMP VALUE 1415.
               10  SCB-B-001416             PIC S9(07)V99 COMP-3 VALUE +0001416.
           05  SCB-GRP-0130.
               10  SCB-C-001417             PIC X(24) VALUE 'FIELD-001417-ALPHA'.
               10  SCB-D-001418             PIC 9(04) VALUE 9926.
               10  SCB-DESC-001418          PIC X(18) VALUE 'DESC-018434'.
               10  SCB-E-001419             OCCURS 5 TIMES.
                   15  SCB-EK-001419        PIC X(10).
                   15  SCB-EV-001419        PIC 9(06) COMP.
               10  SCB-A-001420             PIC 9(09) COMP VALUE 1420.
               10  SCB-B-001421             PIC S9(07)V99 COMP-3 VALUE +0001421.
               10  SCB-C-001422             PIC X(24) VALUE 'FIELD-001422-ALPHA'.
               10  SCB-D-001423             PIC 9(04) VALUE 9961.
               10  SCB-DESC-001423          PIC X(18) VALUE 'DESC-018499'.
               10  SCB-E-001424             OCCURS 2 TIMES.
                   15  SCB-EK-001424        PIC X(10).
                   15  SCB-EV-001424        PIC 9(06) COMP.
               10  SCB-A-001425             PIC 9(09) COMP VALUE 1425.
               10  SCB-B-001426             PIC S9(07)V99 COMP-3 VALUE +0001426.
               10  SCB-C-001427             PIC X(24) VALUE 'FIELD-001427-ALPHA'.
               10  SCB-D-001428             PIC 9(04) VALUE 9996.
               10  SCB-DESC-001428          PIC X(18) VALUE 'DESC-018564'.
           05  SCB-GRP-0131.
               10  SCB-E-001429             OCCURS 3 TIMES.
                   15  SCB-EK-001429        PIC X(10).
                   15  SCB-EV-001429        PIC 9(06) COMP.
               10  SCB-A-001430             PIC 9(09) COMP VALUE 1430.
               10  SCB-B-001431             PIC S9(07)V99 COMP-3 VALUE +0001431.
               10  SCB-C-001432             PIC X(24) VALUE 'FIELD-001432-ALPHA'.
               10  SCB-D-001433             PIC 9(04) VALUE 31.
               10  SCB-DESC-001433          PIC X(18) VALUE 'DESC-018629'.
               10  SCB-E-001434             OCCURS 4 TIMES.
                   15  SCB-EK-001434        PIC X(10).
                   15  SCB-EV-001434        PIC 9(06) COMP.
               10  SCB-A-001435             PIC 9(09) COMP VALUE 1435.
               10  SCB-B-001436             PIC S9(07)V99 COMP-3 VALUE +0001436.
               10  SCB-C-001437             PIC X(24) VALUE 'FIELD-001437-ALPHA'.
               10  SCB-D-001438             PIC 9(04) VALUE 66.
               10  SCB-DESC-001438          PIC X(18) VALUE 'DESC-018694'.
               10  SCB-E-001439             OCCURS 5 TIMES.
                   15  SCB-EK-001439        PIC X(10).
                   15  SCB-EV-001439        PIC 9(06) COMP.
               10  SCB-A-001440             PIC 9(09) COMP VALUE 1440.
               10  SCB-B-001441             PIC S9(07)V99 COMP-3 VALUE +0001441.
           05  SCB-GRP-0132.
               10  SCB-C-001442             PIC X(24) VALUE 'FIELD-001442-ALPHA'.
               10  SCB-D-001443             PIC 9(04) VALUE 101.
               10  SCB-DESC-001443          PIC X(18) VALUE 'DESC-018759'.
               10  SCB-E-001444             OCCURS 2 TIMES.
                   15  SCB-EK-001444        PIC X(10).
                   15  SCB-EV-001444        PIC 9(06) COMP.
               10  SCB-A-001445             PIC 9(09) COMP VALUE 1445.
               10  SCB-B-001446             PIC S9(07)V99 COMP-3 VALUE +0001446.
               10  SCB-C-001447             PIC X(24) VALUE 'FIELD-001447-ALPHA'.
               10  SCB-D-001448             PIC 9(04) VALUE 136.
               10  SCB-DESC-001448          PIC X(18) VALUE 'DESC-018824'.
               10  SCB-E-001449             OCCURS 3 TIMES.
                   15  SCB-EK-001449        PIC X(10).
                   15  SCB-EV-001449        PIC 9(06) COMP.
               10  SCB-A-001450             PIC 9(09) COMP VALUE 1450.
               10  SCB-B-001451             PIC S9(07)V99 COMP-3 VALUE +0001451.
               10  SCB-C-001452             PIC X(24) VALUE 'FIELD-001452-ALPHA'.
               10  SCB-D-001453             PIC 9(04) VALUE 171.
               10  SCB-DESC-001453          PIC X(18) VALUE 'DESC-018889'.
               10  SCB-E-001454             OCCURS 4 TIMES.
                   15  SCB-EK-001454        PIC X(10).
                   15  SCB-EV-001454        PIC 9(06) COMP.
               10  SCB-A-001455             PIC 9(09) COMP VALUE 1455.
           05  SCB-ALT-0022 REDEFINES SCB-GRP-0132.
               10  SCB-ALT-FLAG-0022      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0022   PIC X(64).
           05  SCB-GRP-0133.
               10  SCB-B-001456             PIC S9(07)V99 COMP-3 VALUE +0001456.
               10  SCB-C-001457             PIC X(24) VALUE 'FIELD-001457-ALPHA'.
               10  SCB-D-001458             PIC 9(04) VALUE 206.
               10  SCB-DESC-001458          PIC X(18) VALUE 'DESC-018954'.
               10  SCB-E-001459             OCCURS 5 TIMES.
                   15  SCB-EK-001459        PIC X(10).
                   15  SCB-EV-001459        PIC 9(06) COMP.
               10  SCB-A-001460             PIC 9(09) COMP VALUE 1460.
               10  SCB-B-001461             PIC S9(07)V99 COMP-3 VALUE +0001461.
               10  SCB-C-001462             PIC X(24) VALUE 'FIELD-001462-ALPHA'.
               10  SCB-D-001463             PIC 9(04) VALUE 241.
               10  SCB-DESC-001463          PIC X(18) VALUE 'DESC-019019'.
           05  SCB-GRP-0134.
               10  SCB-E-001464             OCCURS 2 TIMES.
                   15  SCB-EK-001464        PIC X(10).
                   15  SCB-EV-001464        PIC 9(06) COMP.
               10  SCB-A-001465             PIC 9(09) COMP VALUE 1465.
               10  SCB-B-001466             PIC S9(07)V99 COMP-3 VALUE +0001466.
               10  SCB-C-001467             PIC X(24) VALUE 'FIELD-001467-ALPHA'.
               10  SCB-D-001468             PIC 9(04) VALUE 276.
               10  SCB-DESC-001468          PIC X(18) VALUE 'DESC-019084'.
               10  SCB-E-001469             OCCURS 3 TIMES.
                   15  SCB-EK-001469        PIC X(10).
                   15  SCB-EV-001469        PIC 9(06) COMP.
               10  SCB-A-001470             PIC 9(09) COMP VALUE 1470.
               10  SCB-B-001471             PIC S9(07)V99 COMP-3 VALUE +0001471.
               10  SCB-C-001472             PIC X(24) VALUE 'FIELD-001472-ALPHA'.
           05  SCB-GRP-0135.
               10  SCB-D-001473             PIC 9(04) VALUE 311.
               10  SCB-DESC-001473          PIC X(18) VALUE 'DESC-019149'.
               10  SCB-E-001474             OCCURS 4 TIMES.
                   15  SCB-EK-001474        PIC X(10).
                   15  SCB-EV-001474        PIC 9(06) COMP.
               10  SCB-A-001475             PIC 9(09) COMP VALUE 1475.
               10  SCB-B-001476             PIC S9(07)V99 COMP-3 VALUE +0001476.
               10  SCB-C-001477             PIC X(24) VALUE 'FIELD-001477-ALPHA'.
               10  SCB-D-001478             PIC 9(04) VALUE 346.
               10  SCB-DESC-001478          PIC X(18) VALUE 'DESC-019214'.
               10  SCB-E-001479             OCCURS 5 TIMES.
                   15  SCB-EK-001479        PIC X(10).
                   15  SCB-EV-001479        PIC 9(06) COMP.
               10  SCB-A-001480             PIC 9(09) COMP VALUE 1480.
               10  SCB-B-001481             PIC S9(07)V99 COMP-3 VALUE +0001481.
               10  SCB-C-001482             PIC X(24) VALUE 'FIELD-001482-ALPHA'.
           05  SCB-GRP-0136.
               10  SCB-D-001483             PIC 9(04) VALUE 381.
               10  SCB-DESC-001483          PIC X(18) VALUE 'DESC-019279'.
               10  SCB-E-001484             OCCURS 2 TIMES.
                   15  SCB-EK-001484        PIC X(10).
                   15  SCB-EV-001484        PIC 9(06) COMP.
               10  SCB-A-001485             PIC 9(09) COMP VALUE 1485.
               10  SCB-B-001486             PIC S9(07)V99 COMP-3 VALUE +0001486.
               10  SCB-C-001487             PIC X(24) VALUE 'FIELD-001487-ALPHA'.
               10  SCB-D-001488             PIC 9(04) VALUE 416.
               10  SCB-DESC-001488          PIC X(18) VALUE 'DESC-019344'.
               10  SCB-E-001489             OCCURS 3 TIMES.
                   15  SCB-EK-001489        PIC X(10).
                   15  SCB-EV-001489        PIC 9(06) COMP.
               10  SCB-A-001490             PIC 9(09) COMP VALUE 1490.
               10  SCB-B-001491             PIC S9(07)V99 COMP-3 VALUE +0001491.
               10  SCB-C-001492             PIC X(24) VALUE 'FIELD-001492-ALPHA'.
               10  SCB-D-001493             PIC 9(04) VALUE 451.
               10  SCB-DESC-001493          PIC X(18) VALUE 'DESC-019409'.
           05  SCB-GRP-0137.
               10  SCB-E-001494             OCCURS 4 TIMES.
                   15  SCB-EK-001494        PIC X(10).
                   15  SCB-EV-001494        PIC 9(06) COMP.
               10  SCB-A-001495             PIC 9(09) COMP VALUE 1495.
               10  SCB-B-001496             PIC S9(07)V99 COMP-3 VALUE +0001496.
               10  SCB-C-001497             PIC X(24) VALUE 'FIELD-001497-ALPHA'.
               10  SCB-D-001498             PIC 9(04) VALUE 486.
               10  SCB-DESC-001498          PIC X(18) VALUE 'DESC-019474'.
               10  SCB-E-001499             OCCURS 5 TIMES.
                   15  SCB-EK-001499        PIC X(10).
                   15  SCB-EV-001499        PIC 9(06) COMP.
               10  SCB-A-001500             PIC 9(09) COMP VALUE 1500.
               10  SCB-B-001501             PIC S9(07)V99 COMP-3 VALUE +0001501.
               10  SCB-C-001502             PIC X(24) VALUE 'FIELD-001502-ALPHA'.
               10  SCB-D-001503             PIC 9(04) VALUE 521.
               10  SCB-DESC-001503          PIC X(18) VALUE 'DESC-019539'.
               10  SCB-E-001504             OCCURS 2 TIMES.
                   15  SCB-EK-001504        PIC X(10).
                   15  SCB-EV-001504        PIC 9(06) COMP.
               10  SCB-A-001505             PIC 9(09) COMP VALUE 1505.
           05  SCB-GRP-0138.
               10  SCB-B-001506             PIC S9(07)V99 COMP-3 VALUE +0001506.
               10  SCB-C-001507             PIC X(24) VALUE 'FIELD-001507-ALPHA'.
               10  SCB-D-001508             PIC 9(04) VALUE 556.
               10  SCB-DESC-001508          PIC X(18) VALUE 'DESC-019604'.
               10  SCB-E-001509             OCCURS 3 TIMES.
                   15  SCB-EK-001509        PIC X(10).
                   15  SCB-EV-001509        PIC 9(06) COMP.
               10  SCB-A-001510             PIC 9(09) COMP VALUE 1510.
               10  SCB-B-001511             PIC S9(07)V99 COMP-3 VALUE +0001511.
               10  SCB-C-001512             PIC X(24) VALUE 'FIELD-001512-ALPHA'.
               10  SCB-D-001513             PIC 9(04) VALUE 591.
               10  SCB-DESC-001513          PIC X(18) VALUE 'DESC-019669'.
               10  SCB-E-001514             OCCURS 4 TIMES.
                   15  SCB-EK-001514        PIC X(10).
                   15  SCB-EV-001514        PIC 9(06) COMP.
               10  SCB-A-001515             PIC 9(09) COMP VALUE 1515.
               10  SCB-B-001516             PIC S9(07)V99 COMP-3 VALUE +0001516.
               10  SCB-C-001517             PIC X(24) VALUE 'FIELD-001517-ALPHA'.
               10  SCB-D-001518             PIC 9(04) VALUE 626.
               10  SCB-DESC-001518          PIC X(18) VALUE 'DESC-019734'.
           05  SCB-ALT-0023 REDEFINES SCB-GRP-0138.
               10  SCB-ALT-FLAG-0023      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0023   PIC X(64).
           05  SCB-GRP-0139.
               10  SCB-E-001519             OCCURS 5 TIMES.
                   15  SCB-EK-001519        PIC X(10).
                   15  SCB-EV-001519        PIC 9(06) COMP.
               10  SCB-A-001520             PIC 9(09) COMP VALUE 1520.
               10  SCB-B-001521             PIC S9(07)V99 COMP-3 VALUE +0001521.
               10  SCB-C-001522             PIC X(24) VALUE 'FIELD-001522-ALPHA'.
               10  SCB-D-001523             PIC 9(04) VALUE 661.
               10  SCB-DESC-001523          PIC X(18) VALUE 'DESC-019799'.
               10  SCB-E-001524             OCCURS 2 TIMES.
                   15  SCB-EK-001524        PIC X(10).
                   15  SCB-EV-001524        PIC 9(06) COMP.
               10  SCB-A-001525             PIC 9(09) COMP VALUE 1525.
               10  SCB-B-001526             PIC S9(07)V99 COMP-3 VALUE +0001526.
               10  SCB-C-001527             PIC X(24) VALUE 'FIELD-001527-ALPHA'.
               10  SCB-D-001528             PIC 9(04) VALUE 696.
               10  SCB-DESC-001528          PIC X(18) VALUE 'DESC-019864'.
               10  SCB-E-001529             OCCURS 3 TIMES.
                   15  SCB-EK-001529        PIC X(10).
                   15  SCB-EV-001529        PIC 9(06) COMP.
               10  SCB-A-001530             PIC 9(09) COMP VALUE 1530.
               10  SCB-B-001531             PIC S9(07)V99 COMP-3 VALUE +0001531.
               10  SCB-C-001532             PIC X(24) VALUE 'FIELD-001532-ALPHA'.
           05  SCB-GRP-0140.
               10  SCB-D-001533             PIC 9(04) VALUE 731.
               10  SCB-DESC-001533          PIC X(18) VALUE 'DESC-019929'.
               10  SCB-E-001534             OCCURS 4 TIMES.
                   15  SCB-EK-001534        PIC X(10).
                   15  SCB-EV-001534        PIC 9(06) COMP.
               10  SCB-A-001535             PIC 9(09) COMP VALUE 1535.
               10  SCB-B-001536             PIC S9(07)V99 COMP-3 VALUE +0001536.
               10  SCB-C-001537             PIC X(24) VALUE 'FIELD-001537-ALPHA'.
               10  SCB-D-001538             PIC 9(04) VALUE 766.
               10  SCB-DESC-001538          PIC X(18) VALUE 'DESC-019994'.
               10  SCB-E-001539             OCCURS 5 TIMES.
                   15  SCB-EK-001539        PIC X(10).
                   15  SCB-EV-001539        PIC 9(06) COMP.
               10  SCB-A-001540             PIC 9(09) COMP VALUE 1540.
           05  SCB-GRP-0141.
               10  SCB-B-001541             PIC S9(07)V99 COMP-3 VALUE +0001541.
               10  SCB-C-001542             PIC X(24) VALUE 'FIELD-001542-ALPHA'.
               10  SCB-D-001543             PIC 9(04) VALUE 801.
               10  SCB-DESC-001543          PIC X(18) VALUE 'DESC-020059'.
               10  SCB-E-001544             OCCURS 2 TIMES.
                   15  SCB-EK-001544        PIC X(10).
                   15  SCB-EV-001544        PIC 9(06) COMP.
               10  SCB-A-001545             PIC 9(09) COMP VALUE 1545.
               10  SCB-B-001546             PIC S9(07)V99 COMP-3 VALUE +0001546.
               10  SCB-C-001547             PIC X(24) VALUE 'FIELD-001547-ALPHA'.
               10  SCB-D-001548             PIC 9(04) VALUE 836.
               10  SCB-DESC-001548          PIC X(18) VALUE 'DESC-020124'.
               10  SCB-E-001549             OCCURS 3 TIMES.
                   15  SCB-EK-001549        PIC X(10).
                   15  SCB-EV-001549        PIC 9(06) COMP.
           05  SCB-GRP-0142.
               10  SCB-A-001550             PIC 9(09) COMP VALUE 1550.
               10  SCB-B-001551             PIC S9(07)V99 COMP-3 VALUE +0001551.
               10  SCB-C-001552             PIC X(24) VALUE 'FIELD-001552-ALPHA'.
               10  SCB-D-001553             PIC 9(04) VALUE 871.
               10  SCB-DESC-001553          PIC X(18) VALUE 'DESC-020189'.
               10  SCB-E-001554             OCCURS 4 TIMES.
                   15  SCB-EK-001554        PIC X(10).
                   15  SCB-EV-001554        PIC 9(06) COMP.
               10  SCB-A-001555             PIC 9(09) COMP VALUE 1555.
               10  SCB-B-001556             PIC S9(07)V99 COMP-3 VALUE +0001556.
               10  SCB-C-001557             PIC X(24) VALUE 'FIELD-001557-ALPHA'.
               10  SCB-D-001558             PIC 9(04) VALUE 906.
               10  SCB-DESC-001558          PIC X(18) VALUE 'DESC-020254'.
               10  SCB-E-001559             OCCURS 5 TIMES.
                   15  SCB-EK-001559        PIC X(10).
                   15  SCB-EV-001559        PIC 9(06) COMP.
           05  SCB-GRP-0143.
               10  SCB-A-001560             PIC 9(09) COMP VALUE 1560.
               10  SCB-B-001561             PIC S9(07)V99 COMP-3 VALUE +0001561.
               10  SCB-C-001562             PIC X(24) VALUE 'FIELD-001562-ALPHA'.
               10  SCB-D-001563             PIC 9(04) VALUE 941.
               10  SCB-DESC-001563          PIC X(18) VALUE 'DESC-020319'.
               10  SCB-E-001564             OCCURS 2 TIMES.
                   15  SCB-EK-001564        PIC X(10).
                   15  SCB-EV-001564        PIC 9(06) COMP.
               10  SCB-A-001565             PIC 9(09) COMP VALUE 1565.
               10  SCB-B-001566             PIC S9(07)V99 COMP-3 VALUE +0001566.
               10  SCB-C-001567             PIC X(24) VALUE 'FIELD-001567-ALPHA'.
               10  SCB-D-001568             PIC 9(04) VALUE 976.
               10  SCB-DESC-001568          PIC X(18) VALUE 'DESC-020384'.
               10  SCB-E-001569             OCCURS 3 TIMES.
                   15  SCB-EK-001569        PIC X(10).
                   15  SCB-EV-001569        PIC 9(06) COMP.
               10  SCB-A-001570             PIC 9(09) COMP VALUE 1570.
           05  SCB-GRP-0144.
               10  SCB-B-001571             PIC S9(07)V99 COMP-3 VALUE +0001571.
               10  SCB-C-001572             PIC X(24) VALUE 'FIELD-001572-ALPHA'.
               10  SCB-D-001573             PIC 9(04) VALUE 1011.
               10  SCB-DESC-001573          PIC X(18) VALUE 'DESC-020449'.
               10  SCB-E-001574             OCCURS 4 TIMES.
                   15  SCB-EK-001574        PIC X(10).
                   15  SCB-EV-001574        PIC 9(06) COMP.
               10  SCB-A-001575             PIC 9(09) COMP VALUE 1575.
               10  SCB-B-001576             PIC S9(07)V99 COMP-3 VALUE +0001576.
               10  SCB-C-001577             PIC X(24) VALUE 'FIELD-001577-ALPHA'.
               10  SCB-D-001578             PIC 9(04) VALUE 1046.
               10  SCB-DESC-001578          PIC X(18) VALUE 'DESC-020514'.
               10  SCB-E-001579             OCCURS 5 TIMES.
                   15  SCB-EK-001579        PIC X(10).
                   15  SCB-EV-001579        PIC 9(06) COMP.
               10  SCB-A-001580             PIC 9(09) COMP VALUE 1580.
               10  SCB-B-001581             PIC S9(07)V99 COMP-3 VALUE +0001581.
               10  SCB-C-001582             PIC X(24) VALUE 'FIELD-001582-ALPHA'.
           05  SCB-ALT-0024 REDEFINES SCB-GRP-0144.
               10  SCB-ALT-FLAG-0024      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0024   PIC X(64).
           05  SCB-GRP-0145.
               10  SCB-D-001583             PIC 9(04) VALUE 1081.
               10  SCB-DESC-001583          PIC X(18) VALUE 'DESC-020579'.
               10  SCB-E-001584             OCCURS 2 TIMES.
                   15  SCB-EK-001584        PIC X(10).
                   15  SCB-EV-001584        PIC 9(06) COMP.
               10  SCB-A-001585             PIC 9(09) COMP VALUE 1585.
               10  SCB-B-001586             PIC S9(07)V99 COMP-3 VALUE +0001586.
               10  SCB-C-001587             PIC X(24) VALUE 'FIELD-001587-ALPHA'.
               10  SCB-D-001588             PIC 9(04) VALUE 1116.
               10  SCB-DESC-001588          PIC X(18) VALUE 'DESC-020644'.
               10  SCB-E-001589             OCCURS 3 TIMES.
                   15  SCB-EK-001589        PIC X(10).
                   15  SCB-EV-001589        PIC 9(06) COMP.
               10  SCB-A-001590             PIC 9(09) COMP VALUE 1590.
               10  SCB-B-001591             PIC S9(07)V99 COMP-3 VALUE +0001591.
               10  SCB-C-001592             PIC X(24) VALUE 'FIELD-001592-ALPHA'.
               10  SCB-D-001593             PIC 9(04) VALUE 1151.
               10  SCB-DESC-001593          PIC X(18) VALUE 'DESC-020709'.
               10  SCB-E-001594             OCCURS 4 TIMES.
                   15  SCB-EK-001594        PIC X(10).
                   15  SCB-EV-001594        PIC 9(06) COMP.
               10  SCB-A-001595             PIC 9(09) COMP VALUE 1595.
           05  SCB-GRP-0146.
               10  SCB-B-001596             PIC S9(07)V99 COMP-3 VALUE +0001596.
               10  SCB-C-001597             PIC X(24) VALUE 'FIELD-001597-ALPHA'.
               10  SCB-D-001598             PIC 9(04) VALUE 1186.
               10  SCB-DESC-001598          PIC X(18) VALUE 'DESC-020774'.
               10  SCB-E-001599             OCCURS 5 TIMES.
                   15  SCB-EK-001599        PIC X(10).
                   15  SCB-EV-001599        PIC 9(06) COMP.
               10  SCB-A-001600             PIC 9(09) COMP VALUE 1600.
               10  SCB-B-001601             PIC S9(07)V99 COMP-3 VALUE +0001601.
               10  SCB-C-001602             PIC X(24) VALUE 'FIELD-001602-ALPHA'.
               10  SCB-D-001603             PIC 9(04) VALUE 1221.
               10  SCB-DESC-001603          PIC X(18) VALUE 'DESC-020839'.
               10  SCB-E-001604             OCCURS 2 TIMES.
                   15  SCB-EK-001604        PIC X(10).
                   15  SCB-EV-001604        PIC 9(06) COMP.
               10  SCB-A-001605             PIC 9(09) COMP VALUE 1605.
               10  SCB-B-001606             PIC S9(07)V99 COMP-3 VALUE +0001606.
               10  SCB-C-001607             PIC X(24) VALUE 'FIELD-001607-ALPHA'.
               10  SCB-D-001608             PIC 9(04) VALUE 1256.
               10  SCB-DESC-001608          PIC X(18) VALUE 'DESC-020904'.
               10  SCB-E-001609             OCCURS 3 TIMES.
                   15  SCB-EK-001609        PIC X(10).
                   15  SCB-EV-001609        PIC 9(06) COMP.
           05  SCB-GRP-0147.
               10  SCB-A-001610             PIC 9(09) COMP VALUE 1610.
               10  SCB-B-001611             PIC S9(07)V99 COMP-3 VALUE +0001611.
               10  SCB-C-001612             PIC X(24) VALUE 'FIELD-001612-ALPHA'.
               10  SCB-D-001613             PIC 9(04) VALUE 1291.
               10  SCB-DESC-001613          PIC X(18) VALUE 'DESC-020969'.
               10  SCB-E-001614             OCCURS 4 TIMES.
                   15  SCB-EK-001614        PIC X(10).
                   15  SCB-EV-001614        PIC 9(06) COMP.
               10  SCB-A-001615             PIC 9(09) COMP VALUE 1615.
               10  SCB-B-001616             PIC S9(07)V99 COMP-3 VALUE +0001616.
               10  SCB-C-001617             PIC X(24) VALUE 'FIELD-001617-ALPHA'.
           05  SCB-GRP-0148.
               10  SCB-D-001618             PIC 9(04) VALUE 1326.
               10  SCB-DESC-001618          PIC X(18) VALUE 'DESC-021034'.
               10  SCB-E-001619             OCCURS 5 TIMES.
                   15  SCB-EK-001619        PIC X(10).
                   15  SCB-EV-001619        PIC 9(06) COMP.
               10  SCB-A-001620             PIC 9(09) COMP VALUE 1620.
               10  SCB-B-001621             PIC S9(07)V99 COMP-3 VALUE +0001621.
               10  SCB-C-001622             PIC X(24) VALUE 'FIELD-001622-ALPHA'.
               10  SCB-D-001623             PIC 9(04) VALUE 1361.
               10  SCB-DESC-001623          PIC X(18) VALUE 'DESC-021099'.
               10  SCB-E-001624             OCCURS 2 TIMES.
                   15  SCB-EK-001624        PIC X(10).
                   15  SCB-EV-001624        PIC 9(06) COMP.
               10  SCB-A-001625             PIC 9(09) COMP VALUE 1625.
               10  SCB-B-001626             PIC S9(07)V99 COMP-3 VALUE +0001626.
           05  SCB-GRP-0149.
               10  SCB-C-001627             PIC X(24) VALUE 'FIELD-001627-ALPHA'.
               10  SCB-D-001628             PIC 9(04) VALUE 1396.
               10  SCB-DESC-001628          PIC X(18) VALUE 'DESC-021164'.
               10  SCB-E-001629             OCCURS 3 TIMES.
                   15  SCB-EK-001629        PIC X(10).
                   15  SCB-EV-001629        PIC 9(06) COMP.
               10  SCB-A-001630             PIC 9(09) COMP VALUE 1630.
               10  SCB-B-001631             PIC S9(07)V99 COMP-3 VALUE +0001631.
               10  SCB-C-001632             PIC X(24) VALUE 'FIELD-001632-ALPHA'.
               10  SCB-D-001633             PIC 9(04) VALUE 1431.
               10  SCB-DESC-001633          PIC X(18) VALUE 'DESC-021229'.
               10  SCB-E-001634             OCCURS 4 TIMES.
                   15  SCB-EK-001634        PIC X(10).
                   15  SCB-EV-001634        PIC 9(06) COMP.
               10  SCB-A-001635             PIC 9(09) COMP VALUE 1635.
               10  SCB-B-001636             PIC S9(07)V99 COMP-3 VALUE +0001636.
           05  SCB-GRP-0150.
               10  SCB-C-001637             PIC X(24) VALUE 'FIELD-001637-ALPHA'.
               10  SCB-D-001638             PIC 9(04) VALUE 1466.
               10  SCB-DESC-001638          PIC X(18) VALUE 'DESC-021294'.
               10  SCB-E-001639             OCCURS 5 TIMES.
                   15  SCB-EK-001639        PIC X(10).
                   15  SCB-EV-001639        PIC 9(06) COMP.
               10  SCB-A-001640             PIC 9(09) COMP VALUE 1640.
               10  SCB-B-001641             PIC S9(07)V99 COMP-3 VALUE +0001641.
               10  SCB-C-001642             PIC X(24) VALUE 'FIELD-001642-ALPHA'.
               10  SCB-D-001643             PIC 9(04) VALUE 1501.
               10  SCB-DESC-001643          PIC X(18) VALUE 'DESC-021359'.
               10  SCB-E-001644             OCCURS 2 TIMES.
                   15  SCB-EK-001644        PIC X(10).
                   15  SCB-EV-001644        PIC 9(06) COMP.
               10  SCB-A-001645             PIC 9(09) COMP VALUE 1645.
               10  SCB-B-001646             PIC S9(07)V99 COMP-3 VALUE +0001646.
               10  SCB-C-001647             PIC X(24) VALUE 'FIELD-001647-ALPHA'.
           05  SCB-ALT-0025 REDEFINES SCB-GRP-0150.
               10  SCB-ALT-FLAG-0025      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0025   PIC X(64).
           05  SCB-GRP-0151.
               10  SCB-D-001648             PIC 9(04) VALUE 1536.
               10  SCB-DESC-001648          PIC X(18) VALUE 'DESC-021424'.
               10  SCB-E-001649             OCCURS 3 TIMES.
                   15  SCB-EK-001649        PIC X(10).
                   15  SCB-EV-001649        PIC 9(06) COMP.
               10  SCB-A-001650             PIC 9(09) COMP VALUE 1650.
               10  SCB-B-001651             PIC S9(07)V99 COMP-3 VALUE +0001651.
               10  SCB-C-001652             PIC X(24) VALUE 'FIELD-001652-ALPHA'.
               10  SCB-D-001653             PIC 9(04) VALUE 1571.
               10  SCB-DESC-001653          PIC X(18) VALUE 'DESC-021489'.
               10  SCB-E-001654             OCCURS 4 TIMES.
                   15  SCB-EK-001654        PIC X(10).
                   15  SCB-EV-001654        PIC 9(06) COMP.
               10  SCB-A-001655             PIC 9(09) COMP VALUE 1655.
               10  SCB-B-001656             PIC S9(07)V99 COMP-3 VALUE +0001656.
               10  SCB-C-001657             PIC X(24) VALUE 'FIELD-001657-ALPHA'.
               10  SCB-D-001658             PIC 9(04) VALUE 1606.
               10  SCB-DESC-001658          PIC X(18) VALUE 'DESC-021554'.
               10  SCB-E-001659             OCCURS 5 TIMES.
                   15  SCB-EK-001659        PIC X(10).
                   15  SCB-EV-001659        PIC 9(06) COMP.
           05  SCB-GRP-0152.
               10  SCB-A-001660             PIC 9(09) COMP VALUE 1660.
               10  SCB-B-001661             PIC S9(07)V99 COMP-3 VALUE +0001661.
               10  SCB-C-001662             PIC X(24) VALUE 'FIELD-001662-ALPHA'.
               10  SCB-D-001663             PIC 9(04) VALUE 1641.
               10  SCB-DESC-001663          PIC X(18) VALUE 'DESC-021619'.
               10  SCB-E-001664             OCCURS 2 TIMES.
                   15  SCB-EK-001664        PIC X(10).
                   15  SCB-EV-001664        PIC 9(06) COMP.
               10  SCB-A-001665             PIC 9(09) COMP VALUE 1665.
               10  SCB-B-001666             PIC S9(07)V99 COMP-3 VALUE +0001666.
               10  SCB-C-001667             PIC X(24) VALUE 'FIELD-001667-ALPHA'.
               10  SCB-D-001668             PIC 9(04) VALUE 1676.
               10  SCB-DESC-001668          PIC X(18) VALUE 'DESC-021684'.
               10  SCB-E-001669             OCCURS 3 TIMES.
                   15  SCB-EK-001669        PIC X(10).
                   15  SCB-EV-001669        PIC 9(06) COMP.
               10  SCB-A-001670             PIC 9(09) COMP VALUE 1670.
               10  SCB-B-001671             PIC S9(07)V99 COMP-3 VALUE +0001671.
               10  SCB-C-001672             PIC X(24) VALUE 'FIELD-001672-ALPHA'.
           05  SCB-GRP-0153.
               10  SCB-D-001673             PIC 9(04) VALUE 1711.
               10  SCB-DESC-001673          PIC X(18) VALUE 'DESC-021749'.
               10  SCB-E-001674             OCCURS 4 TIMES.
                   15  SCB-EK-001674        PIC X(10).
                   15  SCB-EV-001674        PIC 9(06) COMP.
               10  SCB-A-001675             PIC 9(09) COMP VALUE 1675.
               10  SCB-B-001676             PIC S9(07)V99 COMP-3 VALUE +0001676.
               10  SCB-C-001677             PIC X(24) VALUE 'FIELD-001677-ALPHA'.
               10  SCB-D-001678             PIC 9(04) VALUE 1746.
               10  SCB-DESC-001678          PIC X(18) VALUE 'DESC-021814'.
               10  SCB-E-001679             OCCURS 5 TIMES.
                   15  SCB-EK-001679        PIC X(10).
                   15  SCB-EV-001679        PIC 9(06) COMP.
               10  SCB-A-001680             PIC 9(09) COMP VALUE 1680.
               10  SCB-B-001681             PIC S9(07)V99 COMP-3 VALUE +0001681.
               10  SCB-C-001682             PIC X(24) VALUE 'FIELD-001682-ALPHA'.
               10  SCB-D-001683             PIC 9(04) VALUE 1781.
               10  SCB-DESC-001683          PIC X(18) VALUE 'DESC-021879'.
               10  SCB-E-001684             OCCURS 2 TIMES.
                   15  SCB-EK-001684        PIC X(10).
                   15  SCB-EV-001684        PIC 9(06) COMP.
               10  SCB-A-001685             PIC 9(09) COMP VALUE 1685.
               10  SCB-B-001686             PIC S9(07)V99 COMP-3 VALUE +0001686.
           05  SCB-GRP-0154.
               10  SCB-C-001687             PIC X(24) VALUE 'FIELD-001687-ALPHA'.
               10  SCB-D-001688             PIC 9(04) VALUE 1816.
               10  SCB-DESC-001688          PIC X(18) VALUE 'DESC-021944'.
               10  SCB-E-001689             OCCURS 3 TIMES.
                   15  SCB-EK-001689        PIC X(10).
                   15  SCB-EV-001689        PIC 9(06) COMP.
               10  SCB-A-001690             PIC 9(09) COMP VALUE 1690.
               10  SCB-B-001691             PIC S9(07)V99 COMP-3 VALUE +0001691.
               10  SCB-C-001692             PIC X(24) VALUE 'FIELD-001692-ALPHA'.
               10  SCB-D-001693             PIC 9(04) VALUE 1851.
               10  SCB-DESC-001693          PIC X(18) VALUE 'DESC-022009'.
               10  SCB-E-001694             OCCURS 4 TIMES.
                   15  SCB-EK-001694        PIC X(10).
                   15  SCB-EV-001694        PIC 9(06) COMP.
           05  SCB-GRP-0155.
               10  SCB-A-001695             PIC 9(09) COMP VALUE 1695.
               10  SCB-B-001696             PIC S9(07)V99 COMP-3 VALUE +0001696.
               10  SCB-C-001697             PIC X(24) VALUE 'FIELD-001697-ALPHA'.
               10  SCB-D-001698             PIC 9(04) VALUE 1886.
               10  SCB-DESC-001698          PIC X(18) VALUE 'DESC-022074'.
               10  SCB-E-001699             OCCURS 5 TIMES.
                   15  SCB-EK-001699        PIC X(10).
                   15  SCB-EV-001699        PIC 9(06) COMP.
               10  SCB-A-001700             PIC 9(09) COMP VALUE 1700.
               10  SCB-B-001701             PIC S9(07)V99 COMP-3 VALUE +0001701.
               10  SCB-C-001702             PIC X(24) VALUE 'FIELD-001702-ALPHA'.
               10  SCB-D-001703             PIC 9(04) VALUE 1921.
               10  SCB-DESC-001703          PIC X(18) VALUE 'DESC-022139'.
           05  SCB-GRP-0156.
               10  SCB-E-001704             OCCURS 2 TIMES.
                   15  SCB-EK-001704        PIC X(10).
                   15  SCB-EV-001704        PIC 9(06) COMP.
               10  SCB-A-001705             PIC 9(09) COMP VALUE 1705.
               10  SCB-B-001706             PIC S9(07)V99 COMP-3 VALUE +0001706.
               10  SCB-C-001707             PIC X(24) VALUE 'FIELD-001707-ALPHA'.
               10  SCB-D-001708             PIC 9(04) VALUE 1956.
               10  SCB-DESC-001708          PIC X(18) VALUE 'DESC-022204'.
               10  SCB-E-001709             OCCURS 3 TIMES.
                   15  SCB-EK-001709        PIC X(10).
                   15  SCB-EV-001709        PIC 9(06) COMP.
               10  SCB-A-001710             PIC 9(09) COMP VALUE 1710.
               10  SCB-B-001711             PIC S9(07)V99 COMP-3 VALUE +0001711.
               10  SCB-C-001712             PIC X(24) VALUE 'FIELD-001712-ALPHA'.
               10  SCB-D-001713             PIC 9(04) VALUE 1991.
               10  SCB-DESC-001713          PIC X(18) VALUE 'DESC-022269'.
           05  SCB-ALT-0026 REDEFINES SCB-GRP-0156.
               10  SCB-ALT-FLAG-0026      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0026   PIC X(64).
           05  SCB-GRP-0157.
               10  SCB-E-001714             OCCURS 4 TIMES.
                   15  SCB-EK-001714        PIC X(10).
                   15  SCB-EV-001714        PIC 9(06) COMP.
               10  SCB-A-001715             PIC 9(09) COMP VALUE 1715.
               10  SCB-B-001716             PIC S9(07)V99 COMP-3 VALUE +0001716.
               10  SCB-C-001717             PIC X(24) VALUE 'FIELD-001717-ALPHA'.
               10  SCB-D-001718             PIC 9(04) VALUE 2026.
               10  SCB-DESC-001718          PIC X(18) VALUE 'DESC-022334'.
               10  SCB-E-001719             OCCURS 5 TIMES.
                   15  SCB-EK-001719        PIC X(10).
                   15  SCB-EV-001719        PIC 9(06) COMP.
               10  SCB-A-001720             PIC 9(09) COMP VALUE 1720.
               10  SCB-B-001721             PIC S9(07)V99 COMP-3 VALUE +0001721.
               10  SCB-C-001722             PIC X(24) VALUE 'FIELD-001722-ALPHA'.
               10  SCB-D-001723             PIC 9(04) VALUE 2061.
               10  SCB-DESC-001723          PIC X(18) VALUE 'DESC-022399'.
               10  SCB-E-001724             OCCURS 2 TIMES.
                   15  SCB-EK-001724        PIC X(10).
                   15  SCB-EV-001724        PIC 9(06) COMP.
           05  SCB-GRP-0158.
               10  SCB-A-001725             PIC 9(09) COMP VALUE 1725.
               10  SCB-B-001726             PIC S9(07)V99 COMP-3 VALUE +0001726.
               10  SCB-C-001727             PIC X(24) VALUE 'FIELD-001727-ALPHA'.
               10  SCB-D-001728             PIC 9(04) VALUE 2096.
               10  SCB-DESC-001728          PIC X(18) VALUE 'DESC-022464'.
               10  SCB-E-001729             OCCURS 3 TIMES.
                   15  SCB-EK-001729        PIC X(10).
                   15  SCB-EV-001729        PIC 9(06) COMP.
               10  SCB-A-001730             PIC 9(09) COMP VALUE 1730.
               10  SCB-B-001731             PIC S9(07)V99 COMP-3 VALUE +0001731.
               10  SCB-C-001732             PIC X(24) VALUE 'FIELD-001732-ALPHA'.
               10  SCB-D-001733             PIC 9(04) VALUE 2131.
               10  SCB-DESC-001733          PIC X(18) VALUE 'DESC-022529'.
               10  SCB-E-001734             OCCURS 4 TIMES.
                   15  SCB-EK-001734        PIC X(10).
                   15  SCB-EV-001734        PIC 9(06) COMP.
               10  SCB-A-001735             PIC 9(09) COMP VALUE 1735.
               10  SCB-B-001736             PIC S9(07)V99 COMP-3 VALUE +0001736.
           05  SCB-GRP-0159.
               10  SCB-C-001737             PIC X(24) VALUE 'FIELD-001737-ALPHA'.
               10  SCB-D-001738             PIC 9(04) VALUE 2166.
               10  SCB-DESC-001738          PIC X(18) VALUE 'DESC-022594'.
               10  SCB-E-001739             OCCURS 5 TIMES.
                   15  SCB-EK-001739        PIC X(10).
                   15  SCB-EV-001739        PIC 9(06) COMP.
               10  SCB-A-001740             PIC 9(09) COMP VALUE 1740.
               10  SCB-B-001741             PIC S9(07)V99 COMP-3 VALUE +0001741.
               10  SCB-C-001742             PIC X(24) VALUE 'FIELD-001742-ALPHA'.
               10  SCB-D-001743             PIC 9(04) VALUE 2201.
               10  SCB-DESC-001743          PIC X(18) VALUE 'DESC-022659'.
               10  SCB-E-001744             OCCURS 2 TIMES.
                   15  SCB-EK-001744        PIC X(10).
                   15  SCB-EV-001744        PIC 9(06) COMP.
               10  SCB-A-001745             PIC 9(09) COMP VALUE 1745.
               10  SCB-B-001746             PIC S9(07)V99 COMP-3 VALUE +0001746.
               10  SCB-C-001747             PIC X(24) VALUE 'FIELD-001747-ALPHA'.
               10  SCB-D-001748             PIC 9(04) VALUE 2236.
               10  SCB-DESC-001748          PIC X(18) VALUE 'DESC-022724'.
               10  SCB-E-001749             OCCURS 3 TIMES.
                   15  SCB-EK-001749        PIC X(10).
                   15  SCB-EV-001749        PIC 9(06) COMP.
           05  SCB-GRP-0160.
               10  SCB-A-001750             PIC 9(09) COMP VALUE 1750.
               10  SCB-B-001751             PIC S9(07)V99 COMP-3 VALUE +0001751.
               10  SCB-C-001752             PIC X(24) VALUE 'FIELD-001752-ALPHA'.
               10  SCB-D-001753             PIC 9(04) VALUE 2271.
               10  SCB-DESC-001753          PIC X(18) VALUE 'DESC-022789'.
               10  SCB-E-001754             OCCURS 4 TIMES.
                   15  SCB-EK-001754        PIC X(10).
                   15  SCB-EV-001754        PIC 9(06) COMP.
               10  SCB-A-001755             PIC 9(09) COMP VALUE 1755.
               10  SCB-B-001756             PIC S9(07)V99 COMP-3 VALUE +0001756.
               10  SCB-C-001757             PIC X(24) VALUE 'FIELD-001757-ALPHA'.
               10  SCB-D-001758             PIC 9(04) VALUE 2306.
               10  SCB-DESC-001758          PIC X(18) VALUE 'DESC-022854'.
               10  SCB-E-001759             OCCURS 5 TIMES.
                   15  SCB-EK-001759        PIC X(10).
                   15  SCB-EV-001759        PIC 9(06) COMP.
               10  SCB-A-001760             PIC 9(09) COMP VALUE 1760.
               10  SCB-B-001761             PIC S9(07)V99 COMP-3 VALUE +0001761.
               10  SCB-C-001762             PIC X(24) VALUE 'FIELD-001762-ALPHA'.
               10  SCB-D-001763             PIC 9(04) VALUE 2341.
               10  SCB-DESC-001763          PIC X(18) VALUE 'DESC-022919'.
           05  SCB-GRP-0161.
               10  SCB-E-001764             OCCURS 2 TIMES.
                   15  SCB-EK-001764        PIC X(10).
                   15  SCB-EV-001764        PIC 9(06) COMP.
               10  SCB-A-001765             PIC 9(09) COMP VALUE 1765.
               10  SCB-B-001766             PIC S9(07)V99 COMP-3 VALUE +0001766.
               10  SCB-C-001767             PIC X(24) VALUE 'FIELD-001767-ALPHA'.
               10  SCB-D-001768             PIC 9(04) VALUE 2376.
               10  SCB-DESC-001768          PIC X(18) VALUE 'DESC-022984'.
               10  SCB-E-001769             OCCURS 3 TIMES.
                   15  SCB-EK-001769        PIC X(10).
                   15  SCB-EV-001769        PIC 9(06) COMP.
               10  SCB-A-001770             PIC 9(09) COMP VALUE 1770.
               10  SCB-B-001771             PIC S9(07)V99 COMP-3 VALUE +0001771.
           05  SCB-GRP-0162.
               10  SCB-C-001772             PIC X(24) VALUE 'FIELD-001772-ALPHA'.
               10  SCB-D-001773             PIC 9(04) VALUE 2411.
               10  SCB-DESC-001773          PIC X(18) VALUE 'DESC-023049'.
               10  SCB-E-001774             OCCURS 4 TIMES.
                   15  SCB-EK-001774        PIC X(10).
                   15  SCB-EV-001774        PIC 9(06) COMP.
               10  SCB-A-001775             PIC 9(09) COMP VALUE 1775.
               10  SCB-B-001776             PIC S9(07)V99 COMP-3 VALUE +0001776.
               10  SCB-C-001777             PIC X(24) VALUE 'FIELD-001777-ALPHA'.
               10  SCB-D-001778             PIC 9(04) VALUE 2446.
               10  SCB-DESC-001778          PIC X(18) VALUE 'DESC-023114'.
               10  SCB-E-001779             OCCURS 5 TIMES.
                   15  SCB-EK-001779        PIC X(10).
                   15  SCB-EV-001779        PIC 9(06) COMP.
               10  SCB-A-001780             PIC 9(09) COMP VALUE 1780.
           05  SCB-ALT-0027 REDEFINES SCB-GRP-0162.
               10  SCB-ALT-FLAG-0027      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0027   PIC X(64).
           05  SCB-GRP-0163.
               10  SCB-B-001781             PIC S9(07)V99 COMP-3 VALUE +0001781.
               10  SCB-C-001782             PIC X(24) VALUE 'FIELD-001782-ALPHA'.
               10  SCB-D-001783             PIC 9(04) VALUE 2481.
               10  SCB-DESC-001783          PIC X(18) VALUE 'DESC-023179'.
               10  SCB-E-001784             OCCURS 2 TIMES.
                   15  SCB-EK-001784        PIC X(10).
                   15  SCB-EV-001784        PIC 9(06) COMP.
               10  SCB-A-001785             PIC 9(09) COMP VALUE 1785.
               10  SCB-B-001786             PIC S9(07)V99 COMP-3 VALUE +0001786.
               10  SCB-C-001787             PIC X(24) VALUE 'FIELD-001787-ALPHA'.
               10  SCB-D-001788             PIC 9(04) VALUE 2516.
               10  SCB-DESC-001788          PIC X(18) VALUE 'DESC-023244'.
               10  SCB-E-001789             OCCURS 3 TIMES.
                   15  SCB-EK-001789        PIC X(10).
                   15  SCB-EV-001789        PIC 9(06) COMP.
               10  SCB-A-001790             PIC 9(09) COMP VALUE 1790.
           05  SCB-GRP-0164.
               10  SCB-B-001791             PIC S9(07)V99 COMP-3 VALUE +0001791.
               10  SCB-C-001792             PIC X(24) VALUE 'FIELD-001792-ALPHA'.
               10  SCB-D-001793             PIC 9(04) VALUE 2551.
               10  SCB-DESC-001793          PIC X(18) VALUE 'DESC-023309'.
               10  SCB-E-001794             OCCURS 4 TIMES.
                   15  SCB-EK-001794        PIC X(10).
                   15  SCB-EV-001794        PIC 9(06) COMP.
               10  SCB-A-001795             PIC 9(09) COMP VALUE 1795.
               10  SCB-B-001796             PIC S9(07)V99 COMP-3 VALUE +0001796.
               10  SCB-C-001797             PIC X(24) VALUE 'FIELD-001797-ALPHA'.
               10  SCB-D-001798             PIC 9(04) VALUE 2586.
               10  SCB-DESC-001798          PIC X(18) VALUE 'DESC-023374'.
               10  SCB-E-001799             OCCURS 5 TIMES.
                   15  SCB-EK-001799        PIC X(10).
                   15  SCB-EV-001799        PIC 9(06) COMP.
               10  SCB-A-001800             PIC 9(09) COMP VALUE 1800.
               10  SCB-B-001801             PIC S9(07)V99 COMP-3 VALUE +0001801.
           05  SCB-GRP-0165.
               10  SCB-C-001802             PIC X(24) VALUE 'FIELD-001802-ALPHA'.
               10  SCB-D-001803             PIC 9(04) VALUE 2621.
               10  SCB-DESC-001803          PIC X(18) VALUE 'DESC-023439'.
               10  SCB-E-001804             OCCURS 2 TIMES.
                   15  SCB-EK-001804        PIC X(10).
                   15  SCB-EV-001804        PIC 9(06) COMP.
               10  SCB-A-001805             PIC 9(09) COMP VALUE 1805.
               10  SCB-B-001806             PIC S9(07)V99 COMP-3 VALUE +0001806.
               10  SCB-C-001807             PIC X(24) VALUE 'FIELD-001807-ALPHA'.
               10  SCB-D-001808             PIC 9(04) VALUE 2656.
               10  SCB-DESC-001808          PIC X(18) VALUE 'DESC-023504'.
               10  SCB-E-001809             OCCURS 3 TIMES.
                   15  SCB-EK-001809        PIC X(10).
                   15  SCB-EV-001809        PIC 9(06) COMP.
               10  SCB-A-001810             PIC 9(09) COMP VALUE 1810.
               10  SCB-B-001811             PIC S9(07)V99 COMP-3 VALUE +0001811.
               10  SCB-C-001812             PIC X(24) VALUE 'FIELD-001812-ALPHA'.
               10  SCB-D-001813             PIC 9(04) VALUE 2691.
               10  SCB-DESC-001813          PIC X(18) VALUE 'DESC-023569'.
           05  SCB-GRP-0166.
               10  SCB-E-001814             OCCURS 4 TIMES.
                   15  SCB-EK-001814        PIC X(10).
                   15  SCB-EV-001814        PIC 9(06) COMP.
               10  SCB-A-001815             PIC 9(09) COMP VALUE 1815.
               10  SCB-B-001816             PIC S9(07)V99 COMP-3 VALUE +0001816.
               10  SCB-C-001817             PIC X(24) VALUE 'FIELD-001817-ALPHA'.
               10  SCB-D-001818             PIC 9(04) VALUE 2726.
               10  SCB-DESC-001818          PIC X(18) VALUE 'DESC-023634'.
               10  SCB-E-001819             OCCURS 5 TIMES.
                   15  SCB-EK-001819        PIC X(10).
                   15  SCB-EV-001819        PIC 9(06) COMP.
               10  SCB-A-001820             PIC 9(09) COMP VALUE 1820.
               10  SCB-B-001821             PIC S9(07)V99 COMP-3 VALUE +0001821.
               10  SCB-C-001822             PIC X(24) VALUE 'FIELD-001822-ALPHA'.
               10  SCB-D-001823             PIC 9(04) VALUE 2761.
               10  SCB-DESC-001823          PIC X(18) VALUE 'DESC-023699'.
               10  SCB-E-001824             OCCURS 2 TIMES.
                   15  SCB-EK-001824        PIC X(10).
                   15  SCB-EV-001824        PIC 9(06) COMP.
               10  SCB-A-001825             PIC 9(09) COMP VALUE 1825.
               10  SCB-B-001826             PIC S9(07)V99 COMP-3 VALUE +0001826.
           05  SCB-GRP-0167.
               10  SCB-C-001827             PIC X(24) VALUE 'FIELD-001827-ALPHA'.
               10  SCB-D-001828             PIC 9(04) VALUE 2796.
               10  SCB-DESC-001828          PIC X(18) VALUE 'DESC-023764'.
               10  SCB-E-001829             OCCURS 3 TIMES.
                   15  SCB-EK-001829        PIC X(10).
                   15  SCB-EV-001829        PIC 9(06) COMP.
               10  SCB-A-001830             PIC 9(09) COMP VALUE 1830.
               10  SCB-B-001831             PIC S9(07)V99 COMP-3 VALUE +0001831.
               10  SCB-C-001832             PIC X(24) VALUE 'FIELD-001832-ALPHA'.
               10  SCB-D-001833             PIC 9(04) VALUE 2831.
               10  SCB-DESC-001833          PIC X(18) VALUE 'DESC-023829'.
               10  SCB-E-001834             OCCURS 4 TIMES.
                   15  SCB-EK-001834        PIC X(10).
                   15  SCB-EV-001834        PIC 9(06) COMP.
               10  SCB-A-001835             PIC 9(09) COMP VALUE 1835.
               10  SCB-B-001836             PIC S9(07)V99 COMP-3 VALUE +0001836.
               10  SCB-C-001837             PIC X(24) VALUE 'FIELD-001837-ALPHA'.
               10  SCB-D-001838             PIC 9(04) VALUE 2866.
               10  SCB-DESC-001838          PIC X(18) VALUE 'DESC-023894'.
               10  SCB-E-001839             OCCURS 5 TIMES.
                   15  SCB-EK-001839        PIC X(10).
                   15  SCB-EV-001839        PIC 9(06) COMP.
               10  SCB-A-001840             PIC 9(09) COMP VALUE 1840.
           05  SCB-GRP-0168.
               10  SCB-B-001841             PIC S9(07)V99 COMP-3 VALUE +0001841.
               10  SCB-C-001842             PIC X(24) VALUE 'FIELD-001842-ALPHA'.
               10  SCB-D-001843             PIC 9(04) VALUE 2901.
               10  SCB-DESC-001843          PIC X(18) VALUE 'DESC-023959'.
               10  SCB-E-001844             OCCURS 2 TIMES.
                   15  SCB-EK-001844        PIC X(10).
                   15  SCB-EV-001844        PIC 9(06) COMP.
               10  SCB-A-001845             PIC 9(09) COMP VALUE 1845.
               10  SCB-B-001846             PIC S9(07)V99 COMP-3 VALUE +0001846.
               10  SCB-C-001847             PIC X(24) VALUE 'FIELD-001847-ALPHA'.
               10  SCB-D-001848             PIC 9(04) VALUE 2936.
               10  SCB-DESC-001848          PIC X(18) VALUE 'DESC-024024'.
           05  SCB-ALT-0028 REDEFINES SCB-GRP-0168.
               10  SCB-ALT-FLAG-0028      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0028   PIC X(64).
           05  SCB-GRP-0169.
               10  SCB-E-001849             OCCURS 3 TIMES.
                   15  SCB-EK-001849        PIC X(10).
                   15  SCB-EV-001849        PIC 9(06) COMP.
               10  SCB-A-001850             PIC 9(09) COMP VALUE 1850.
               10  SCB-B-001851             PIC S9(07)V99 COMP-3 VALUE +0001851.
               10  SCB-C-001852             PIC X(24) VALUE 'FIELD-001852-ALPHA'.
               10  SCB-D-001853             PIC 9(04) VALUE 2971.
               10  SCB-DESC-001853          PIC X(18) VALUE 'DESC-024089'.
               10  SCB-E-001854             OCCURS 4 TIMES.
                   15  SCB-EK-001854        PIC X(10).
                   15  SCB-EV-001854        PIC 9(06) COMP.
               10  SCB-A-001855             PIC 9(09) COMP VALUE 1855.
               10  SCB-B-001856             PIC S9(07)V99 COMP-3 VALUE +0001856.
               10  SCB-C-001857             PIC X(24) VALUE 'FIELD-001857-ALPHA'.
           05  SCB-GRP-0170.
               10  SCB-D-001858             PIC 9(04) VALUE 3006.
               10  SCB-DESC-001858          PIC X(18) VALUE 'DESC-024154'.
               10  SCB-E-001859             OCCURS 5 TIMES.
                   15  SCB-EK-001859        PIC X(10).
                   15  SCB-EV-001859        PIC 9(06) COMP.
               10  SCB-A-001860             PIC 9(09) COMP VALUE 1860.
               10  SCB-B-001861             PIC S9(07)V99 COMP-3 VALUE +0001861.
               10  SCB-C-001862             PIC X(24) VALUE 'FIELD-001862-ALPHA'.
               10  SCB-D-001863             PIC 9(04) VALUE 3041.
               10  SCB-DESC-001863          PIC X(18) VALUE 'DESC-024219'.
               10  SCB-E-001864             OCCURS 2 TIMES.
                   15  SCB-EK-001864        PIC X(10).
                   15  SCB-EV-001864        PIC 9(06) COMP.
               10  SCB-A-001865             PIC 9(09) COMP VALUE 1865.
               10  SCB-B-001866             PIC S9(07)V99 COMP-3 VALUE +0001866.
               10  SCB-C-001867             PIC X(24) VALUE 'FIELD-001867-ALPHA'.
           05  SCB-GRP-0171.
               10  SCB-D-001868             PIC 9(04) VALUE 3076.
               10  SCB-DESC-001868          PIC X(18) VALUE 'DESC-024284'.
               10  SCB-E-001869             OCCURS 3 TIMES.
                   15  SCB-EK-001869        PIC X(10).
                   15  SCB-EV-001869        PIC 9(06) COMP.
               10  SCB-A-001870             PIC 9(09) COMP VALUE 1870.
               10  SCB-B-001871             PIC S9(07)V99 COMP-3 VALUE +0001871.
               10  SCB-C-001872             PIC X(24) VALUE 'FIELD-001872-ALPHA'.
               10  SCB-D-001873             PIC 9(04) VALUE 3111.
               10  SCB-DESC-001873          PIC X(18) VALUE 'DESC-024349'.
               10  SCB-E-001874             OCCURS 4 TIMES.
                   15  SCB-EK-001874        PIC X(10).
                   15  SCB-EV-001874        PIC 9(06) COMP.
               10  SCB-A-001875             PIC 9(09) COMP VALUE 1875.
               10  SCB-B-001876             PIC S9(07)V99 COMP-3 VALUE +0001876.
               10  SCB-C-001877             PIC X(24) VALUE 'FIELD-001877-ALPHA'.
               10  SCB-D-001878             PIC 9(04) VALUE 3146.
               10  SCB-DESC-001878          PIC X(18) VALUE 'DESC-024414'.
           05  SCB-GRP-0172.
               10  SCB-E-001879             OCCURS 5 TIMES.
                   15  SCB-EK-001879        PIC X(10).
                   15  SCB-EV-001879        PIC 9(06) COMP.
               10  SCB-A-001880             PIC 9(09) COMP VALUE 1880.
               10  SCB-B-001881             PIC S9(07)V99 COMP-3 VALUE +0001881.
               10  SCB-C-001882             PIC X(24) VALUE 'FIELD-001882-ALPHA'.
               10  SCB-D-001883             PIC 9(04) VALUE 3181.
               10  SCB-DESC-001883          PIC X(18) VALUE 'DESC-024479'.
               10  SCB-E-001884             OCCURS 2 TIMES.
                   15  SCB-EK-001884        PIC X(10).
                   15  SCB-EV-001884        PIC 9(06) COMP.
               10  SCB-A-001885             PIC 9(09) COMP VALUE 1885.
               10  SCB-B-001886             PIC S9(07)V99 COMP-3 VALUE +0001886.
               10  SCB-C-001887             PIC X(24) VALUE 'FIELD-001887-ALPHA'.
               10  SCB-D-001888             PIC 9(04) VALUE 3216.
               10  SCB-DESC-001888          PIC X(18) VALUE 'DESC-024544'.
               10  SCB-E-001889             OCCURS 3 TIMES.
                   15  SCB-EK-001889        PIC X(10).
                   15  SCB-EV-001889        PIC 9(06) COMP.
               10  SCB-A-001890             PIC 9(09) COMP VALUE 1890.
           05  SCB-GRP-0173.
               10  SCB-B-001891             PIC S9(07)V99 COMP-3 VALUE +0001891.
               10  SCB-C-001892             PIC X(24) VALUE 'FIELD-001892-ALPHA'.
               10  SCB-D-001893             PIC 9(04) VALUE 3251.
               10  SCB-DESC-001893          PIC X(18) VALUE 'DESC-024609'.
               10  SCB-E-001894             OCCURS 4 TIMES.
                   15  SCB-EK-001894        PIC X(10).
                   15  SCB-EV-001894        PIC 9(06) COMP.
               10  SCB-A-001895             PIC 9(09) COMP VALUE 1895.
               10  SCB-B-001896             PIC S9(07)V99 COMP-3 VALUE +0001896.
               10  SCB-C-001897             PIC X(24) VALUE 'FIELD-001897-ALPHA'.
               10  SCB-D-001898             PIC 9(04) VALUE 3286.
               10  SCB-DESC-001898          PIC X(18) VALUE 'DESC-024674'.
               10  SCB-E-001899             OCCURS 5 TIMES.
                   15  SCB-EK-001899        PIC X(10).
                   15  SCB-EV-001899        PIC 9(06) COMP.
               10  SCB-A-001900             PIC 9(09) COMP VALUE 1900.
               10  SCB-B-001901             PIC S9(07)V99 COMP-3 VALUE +0001901.
               10  SCB-C-001902             PIC X(24) VALUE 'FIELD-001902-ALPHA'.
               10  SCB-D-001903             PIC 9(04) VALUE 3321.
               10  SCB-DESC-001903          PIC X(18) VALUE 'DESC-024739'.
           05  SCB-GRP-0174.
               10  SCB-E-001904             OCCURS 2 TIMES.
                   15  SCB-EK-001904        PIC X(10).
                   15  SCB-EV-001904        PIC 9(06) COMP.
               10  SCB-A-001905             PIC 9(09) COMP VALUE 1905.
               10  SCB-B-001906             PIC S9(07)V99 COMP-3 VALUE +0001906.
               10  SCB-C-001907             PIC X(24) VALUE 'FIELD-001907-ALPHA'.
               10  SCB-D-001908             PIC 9(04) VALUE 3356.
               10  SCB-DESC-001908          PIC X(18) VALUE 'DESC-024804'.
               10  SCB-E-001909             OCCURS 3 TIMES.
                   15  SCB-EK-001909        PIC X(10).
                   15  SCB-EV-001909        PIC 9(06) COMP.
               10  SCB-A-001910             PIC 9(09) COMP VALUE 1910.
               10  SCB-B-001911             PIC S9(07)V99 COMP-3 VALUE +0001911.
               10  SCB-C-001912             PIC X(24) VALUE 'FIELD-001912-ALPHA'.
               10  SCB-D-001913             PIC 9(04) VALUE 3391.
               10  SCB-DESC-001913          PIC X(18) VALUE 'DESC-024869'.
               10  SCB-E-001914             OCCURS 4 TIMES.
                   15  SCB-EK-001914        PIC X(10).
                   15  SCB-EV-001914        PIC 9(06) COMP.
               10  SCB-A-001915             PIC 9(09) COMP VALUE 1915.
               10  SCB-B-001916             PIC S9(07)V99 COMP-3 VALUE +0001916.
               10  SCB-C-001917             PIC X(24) VALUE 'FIELD-001917-ALPHA'.
           05  SCB-ALT-0029 REDEFINES SCB-GRP-0174.
               10  SCB-ALT-FLAG-0029      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0029   PIC X(64).
           05  SCB-GRP-0175.
               10  SCB-D-001918             PIC 9(04) VALUE 3426.
               10  SCB-DESC-001918          PIC X(18) VALUE 'DESC-024934'.
               10  SCB-E-001919             OCCURS 5 TIMES.
                   15  SCB-EK-001919        PIC X(10).
                   15  SCB-EV-001919        PIC 9(06) COMP.
               10  SCB-A-001920             PIC 9(09) COMP VALUE 1920.
               10  SCB-B-001921             PIC S9(07)V99 COMP-3 VALUE +0001921.
               10  SCB-C-001922             PIC X(24) VALUE 'FIELD-001922-ALPHA'.
               10  SCB-D-001923             PIC 9(04) VALUE 3461.
               10  SCB-DESC-001923          PIC X(18) VALUE 'DESC-024999'.
               10  SCB-E-001924             OCCURS 2 TIMES.
                   15  SCB-EK-001924        PIC X(10).
                   15  SCB-EV-001924        PIC 9(06) COMP.
               10  SCB-A-001925             PIC 9(09) COMP VALUE 1925.
           05  SCB-GRP-0176.
               10  SCB-B-001926             PIC S9(07)V99 COMP-3 VALUE +0001926.
               10  SCB-C-001927             PIC X(24) VALUE 'FIELD-001927-ALPHA'.
               10  SCB-D-001928             PIC 9(04) VALUE 3496.
               10  SCB-DESC-001928          PIC X(18) VALUE 'DESC-025064'.
               10  SCB-E-001929             OCCURS 3 TIMES.
                   15  SCB-EK-001929        PIC X(10).
                   15  SCB-EV-001929        PIC 9(06) COMP.
               10  SCB-A-001930             PIC 9(09) COMP VALUE 1930.
               10  SCB-B-001931             PIC S9(07)V99 COMP-3 VALUE +0001931.
               10  SCB-C-001932             PIC X(24) VALUE 'FIELD-001932-ALPHA'.
               10  SCB-D-001933             PIC 9(04) VALUE 3531.
               10  SCB-DESC-001933          PIC X(18) VALUE 'DESC-025129'.
               10  SCB-E-001934             OCCURS 4 TIMES.
                   15  SCB-EK-001934        PIC X(10).
                   15  SCB-EV-001934        PIC 9(06) COMP.
           05  SCB-GRP-0177.
               10  SCB-A-001935             PIC 9(09) COMP VALUE 1935.
               10  SCB-B-001936             PIC S9(07)V99 COMP-3 VALUE +0001936.
               10  SCB-C-001937             PIC X(24) VALUE 'FIELD-001937-ALPHA'.
               10  SCB-D-001938             PIC 9(04) VALUE 3566.
               10  SCB-DESC-001938          PIC X(18) VALUE 'DESC-025194'.
               10  SCB-E-001939             OCCURS 5 TIMES.
                   15  SCB-EK-001939        PIC X(10).
                   15  SCB-EV-001939        PIC 9(06) COMP.
               10  SCB-A-001940             PIC 9(09) COMP VALUE 1940.
               10  SCB-B-001941             PIC S9(07)V99 COMP-3 VALUE +0001941.
               10  SCB-C-001942             PIC X(24) VALUE 'FIELD-001942-ALPHA'.
               10  SCB-D-001943             PIC 9(04) VALUE 3601.
               10  SCB-DESC-001943          PIC X(18) VALUE 'DESC-025259'.
               10  SCB-E-001944             OCCURS 2 TIMES.
                   15  SCB-EK-001944        PIC X(10).
                   15  SCB-EV-001944        PIC 9(06) COMP.
           05  SCB-GRP-0178.
               10  SCB-A-001945             PIC 9(09) COMP VALUE 1945.
               10  SCB-B-001946             PIC S9(07)V99 COMP-3 VALUE +0001946.
               10  SCB-C-001947             PIC X(24) VALUE 'FIELD-001947-ALPHA'.
               10  SCB-D-001948             PIC 9(04) VALUE 3636.
               10  SCB-DESC-001948          PIC X(18) VALUE 'DESC-025324'.
               10  SCB-E-001949             OCCURS 3 TIMES.
                   15  SCB-EK-001949        PIC X(10).
                   15  SCB-EV-001949        PIC 9(06) COMP.
               10  SCB-A-001950             PIC 9(09) COMP VALUE 1950.
               10  SCB-B-001951             PIC S9(07)V99 COMP-3 VALUE +0001951.
               10  SCB-C-001952             PIC X(24) VALUE 'FIELD-001952-ALPHA'.
               10  SCB-D-001953             PIC 9(04) VALUE 3671.
               10  SCB-DESC-001953          PIC X(18) VALUE 'DESC-025389'.
               10  SCB-E-001954             OCCURS 4 TIMES.
                   15  SCB-EK-001954        PIC X(10).
                   15  SCB-EV-001954        PIC 9(06) COMP.
               10  SCB-A-001955             PIC 9(09) COMP VALUE 1955.
           05  SCB-GRP-0179.
               10  SCB-B-001956             PIC S9(07)V99 COMP-3 VALUE +0001956.
               10  SCB-C-001957             PIC X(24) VALUE 'FIELD-001957-ALPHA'.
               10  SCB-D-001958             PIC 9(04) VALUE 3706.
               10  SCB-DESC-001958          PIC X(18) VALUE 'DESC-025454'.
               10  SCB-E-001959             OCCURS 5 TIMES.
                   15  SCB-EK-001959        PIC X(10).
                   15  SCB-EV-001959        PIC 9(06) COMP.
               10  SCB-A-001960             PIC 9(09) COMP VALUE 1960.
               10  SCB-B-001961             PIC S9(07)V99 COMP-3 VALUE +0001961.
               10  SCB-C-001962             PIC X(24) VALUE 'FIELD-001962-ALPHA'.
               10  SCB-D-001963             PIC 9(04) VALUE 3741.
               10  SCB-DESC-001963          PIC X(18) VALUE 'DESC-025519'.
               10  SCB-E-001964             OCCURS 2 TIMES.
                   15  SCB-EK-001964        PIC X(10).
                   15  SCB-EV-001964        PIC 9(06) COMP.
               10  SCB-A-001965             PIC 9(09) COMP VALUE 1965.
               10  SCB-B-001966             PIC S9(07)V99 COMP-3 VALUE +0001966.
               10  SCB-C-001967             PIC X(24) VALUE 'FIELD-001967-ALPHA'.
           05  SCB-GRP-0180.
               10  SCB-D-001968             PIC 9(04) VALUE 3776.
               10  SCB-DESC-001968          PIC X(18) VALUE 'DESC-025584'.
               10  SCB-E-001969             OCCURS 3 TIMES.
                   15  SCB-EK-001969        PIC X(10).
                   15  SCB-EV-001969        PIC 9(06) COMP.
               10  SCB-A-001970             PIC 9(09) COMP VALUE 1970.
               10  SCB-B-001971             PIC S9(07)V99 COMP-3 VALUE +0001971.
               10  SCB-C-001972             PIC X(24) VALUE 'FIELD-001972-ALPHA'.
               10  SCB-D-001973             PIC 9(04) VALUE 3811.
               10  SCB-DESC-001973          PIC X(18) VALUE 'DESC-025649'.
               10  SCB-E-001974             OCCURS 4 TIMES.
                   15  SCB-EK-001974        PIC X(10).
                   15  SCB-EV-001974        PIC 9(06) COMP.
               10  SCB-A-001975             PIC 9(09) COMP VALUE 1975.
               10  SCB-B-001976             PIC S9(07)V99 COMP-3 VALUE +0001976.
               10  SCB-C-001977             PIC X(24) VALUE 'FIELD-001977-ALPHA'.
               10  SCB-D-001978             PIC 9(04) VALUE 3846.
               10  SCB-DESC-001978          PIC X(18) VALUE 'DESC-025714'.
               10  SCB-E-001979             OCCURS 5 TIMES.
                   15  SCB-EK-001979        PIC X(10).
                   15  SCB-EV-001979        PIC 9(06) COMP.
               10  SCB-A-001980             PIC 9(09) COMP VALUE 1980.
           05  SCB-ALT-0030 REDEFINES SCB-GRP-0180.
               10  SCB-ALT-FLAG-0030      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0030   PIC X(64).
           05  SCB-GRP-0181.
               10  SCB-B-001981             PIC S9(07)V99 COMP-3 VALUE +0001981.
               10  SCB-C-001982             PIC X(24) VALUE 'FIELD-001982-ALPHA'.
               10  SCB-D-001983             PIC 9(04) VALUE 3881.
               10  SCB-DESC-001983          PIC X(18) VALUE 'DESC-025779'.
               10  SCB-E-001984             OCCURS 2 TIMES.
                   15  SCB-EK-001984        PIC X(10).
                   15  SCB-EV-001984        PIC 9(06) COMP.
               10  SCB-A-001985             PIC 9(09) COMP VALUE 1985.
               10  SCB-B-001986             PIC S9(07)V99 COMP-3 VALUE +0001986.
               10  SCB-C-001987             PIC X(24) VALUE 'FIELD-001987-ALPHA'.
               10  SCB-D-001988             PIC 9(04) VALUE 3916.
               10  SCB-DESC-001988          PIC X(18) VALUE 'DESC-025844'.
               10  SCB-E-001989             OCCURS 3 TIMES.
                   15  SCB-EK-001989        PIC X(10).
                   15  SCB-EV-001989        PIC 9(06) COMP.
               10  SCB-A-001990             PIC 9(09) COMP VALUE 1990.
               10  SCB-B-001991             PIC S9(07)V99 COMP-3 VALUE +0001991.
               10  SCB-C-001992             PIC X(24) VALUE 'FIELD-001992-ALPHA'.
               10  SCB-D-001993             PIC 9(04) VALUE 3951.
               10  SCB-DESC-001993          PIC X(18) VALUE 'DESC-025909'.
               10  SCB-E-001994             OCCURS 4 TIMES.
                   15  SCB-EK-001994        PIC X(10).
                   15  SCB-EV-001994        PIC 9(06) COMP.
           05  SCB-GRP-0182.
               10  SCB-A-001995             PIC 9(09) COMP VALUE 1995.
               10  SCB-B-001996             PIC S9(07)V99 COMP-3 VALUE +0001996.
               10  SCB-C-001997             PIC X(24) VALUE 'FIELD-001997-ALPHA'.
               10  SCB-D-001998             PIC 9(04) VALUE 3986.
               10  SCB-DESC-001998          PIC X(18) VALUE 'DESC-025974'.
               10  SCB-E-001999             OCCURS 5 TIMES.
                   15  SCB-EK-001999        PIC X(10).
                   15  SCB-EV-001999        PIC 9(06) COMP.
               10  SCB-A-002000             PIC 9(09) COMP VALUE 2000.
               10  SCB-B-002001             PIC S9(07)V99 COMP-3 VALUE +0002001.
               10  SCB-C-002002             PIC X(24) VALUE 'FIELD-002002-ALPHA'.
           05  SCB-GRP-0183.
               10  SCB-D-002003             PIC 9(04) VALUE 4021.
               10  SCB-DESC-002003          PIC X(18) VALUE 'DESC-026039'.
               10  SCB-E-002004             OCCURS 2 TIMES.
                   15  SCB-EK-002004        PIC X(10).
                   15  SCB-EV-002004        PIC 9(06) COMP.
               10  SCB-A-002005             PIC 9(09) COMP VALUE 2005.
               10  SCB-B-002006             PIC S9(07)V99 COMP-3 VALUE +0002006.
               10  SCB-C-002007             PIC X(24) VALUE 'FIELD-002007-ALPHA'.
               10  SCB-D-002008             PIC 9(04) VALUE 4056.
               10  SCB-DESC-002008          PIC X(18) VALUE 'DESC-026104'.
               10  SCB-E-002009             OCCURS 3 TIMES.
                   15  SCB-EK-002009        PIC X(10).
                   15  SCB-EV-002009        PIC 9(06) COMP.
               10  SCB-A-002010             PIC 9(09) COMP VALUE 2010.
               10  SCB-B-002011             PIC S9(07)V99 COMP-3 VALUE +0002011.
           05  SCB-GRP-0184.
               10  SCB-C-002012             PIC X(24) VALUE 'FIELD-002012-ALPHA'.
               10  SCB-D-002013             PIC 9(04) VALUE 4091.
               10  SCB-DESC-002013          PIC X(18) VALUE 'DESC-026169'.
               10  SCB-E-002014             OCCURS 4 TIMES.
                   15  SCB-EK-002014        PIC X(10).
                   15  SCB-EV-002014        PIC 9(06) COMP.
               10  SCB-A-002015             PIC 9(09) COMP VALUE 2015.
               10  SCB-B-002016             PIC S9(07)V99 COMP-3 VALUE +0002016.
               10  SCB-C-002017             PIC X(24) VALUE 'FIELD-002017-ALPHA'.
               10  SCB-D-002018             PIC 9(04) VALUE 4126.
               10  SCB-DESC-002018          PIC X(18) VALUE 'DESC-026234'.
               10  SCB-E-002019             OCCURS 5 TIMES.
                   15  SCB-EK-002019        PIC X(10).
                   15  SCB-EV-002019        PIC 9(06) COMP.
               10  SCB-A-002020             PIC 9(09) COMP VALUE 2020.
               10  SCB-B-002021             PIC S9(07)V99 COMP-3 VALUE +0002021.
           05  SCB-GRP-0185.
               10  SCB-C-002022             PIC X(24) VALUE 'FIELD-002022-ALPHA'.
               10  SCB-D-002023             PIC 9(04) VALUE 4161.
               10  SCB-DESC-002023          PIC X(18) VALUE 'DESC-026299'.
               10  SCB-E-002024             OCCURS 2 TIMES.
                   15  SCB-EK-002024        PIC X(10).
                   15  SCB-EV-002024        PIC 9(06) COMP.
               10  SCB-A-002025             PIC 9(09) COMP VALUE 2025.
               10  SCB-B-002026             PIC S9(07)V99 COMP-3 VALUE +0002026.
               10  SCB-C-002027             PIC X(24) VALUE 'FIELD-002027-ALPHA'.
               10  SCB-D-002028             PIC 9(04) VALUE 4196.
               10  SCB-DESC-002028          PIC X(18) VALUE 'DESC-026364'.
               10  SCB-E-002029             OCCURS 3 TIMES.
                   15  SCB-EK-002029        PIC X(10).
                   15  SCB-EV-002029        PIC 9(06) COMP.
               10  SCB-A-002030             PIC 9(09) COMP VALUE 2030.
               10  SCB-B-002031             PIC S9(07)V99 COMP-3 VALUE +0002031.
               10  SCB-C-002032             PIC X(24) VALUE 'FIELD-002032-ALPHA'.
           05  SCB-GRP-0186.
               10  SCB-D-002033             PIC 9(04) VALUE 4231.
               10  SCB-DESC-002033          PIC X(18) VALUE 'DESC-026429'.
               10  SCB-E-002034             OCCURS 4 TIMES.
                   15  SCB-EK-002034        PIC X(10).
                   15  SCB-EV-002034        PIC 9(06) COMP.
               10  SCB-A-002035             PIC 9(09) COMP VALUE 2035.
               10  SCB-B-002036             PIC S9(07)V99 COMP-3 VALUE +0002036.
               10  SCB-C-002037             PIC X(24) VALUE 'FIELD-002037-ALPHA'.
               10  SCB-D-002038             PIC 9(04) VALUE 4266.
               10  SCB-DESC-002038          PIC X(18) VALUE 'DESC-026494'.
               10  SCB-E-002039             OCCURS 5 TIMES.
                   15  SCB-EK-002039        PIC X(10).
                   15  SCB-EV-002039        PIC 9(06) COMP.
               10  SCB-A-002040             PIC 9(09) COMP VALUE 2040.
               10  SCB-B-002041             PIC S9(07)V99 COMP-3 VALUE +0002041.
               10  SCB-C-002042             PIC X(24) VALUE 'FIELD-002042-ALPHA'.
               10  SCB-D-002043             PIC 9(04) VALUE 4301.
               10  SCB-DESC-002043          PIC X(18) VALUE 'DESC-026559'.
               10  SCB-E-002044             OCCURS 2 TIMES.
                   15  SCB-EK-002044        PIC X(10).
                   15  SCB-EV-002044        PIC 9(06) COMP.
           05  SCB-ALT-0031 REDEFINES SCB-GRP-0186.
               10  SCB-ALT-FLAG-0031      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0031   PIC X(64).
           05  SCB-GRP-0187.
               10  SCB-A-002045             PIC 9(09) COMP VALUE 2045.
               10  SCB-B-002046             PIC S9(07)V99 COMP-3 VALUE +0002046.
               10  SCB-C-002047             PIC X(24) VALUE 'FIELD-002047-ALPHA'.
               10  SCB-D-002048             PIC 9(04) VALUE 4336.
               10  SCB-DESC-002048          PIC X(18) VALUE 'DESC-026624'.
               10  SCB-E-002049             OCCURS 3 TIMES.
                   15  SCB-EK-002049        PIC X(10).
                   15  SCB-EV-002049        PIC 9(06) COMP.
               10  SCB-A-002050             PIC 9(09) COMP VALUE 2050.
               10  SCB-B-002051             PIC S9(07)V99 COMP-3 VALUE +0002051.
               10  SCB-C-002052             PIC X(24) VALUE 'FIELD-002052-ALPHA'.
               10  SCB-D-002053             PIC 9(04) VALUE 4371.
               10  SCB-DESC-002053          PIC X(18) VALUE 'DESC-026689'.
               10  SCB-E-002054             OCCURS 4 TIMES.
                   15  SCB-EK-002054        PIC X(10).
                   15  SCB-EV-002054        PIC 9(06) COMP.
               10  SCB-A-002055             PIC 9(09) COMP VALUE 2055.
               10  SCB-B-002056             PIC S9(07)V99 COMP-3 VALUE +0002056.
               10  SCB-C-002057             PIC X(24) VALUE 'FIELD-002057-ALPHA'.
           05  SCB-GRP-0188.
               10  SCB-D-002058             PIC 9(04) VALUE 4406.
               10  SCB-DESC-002058          PIC X(18) VALUE 'DESC-026754'.
               10  SCB-E-002059             OCCURS 5 TIMES.
                   15  SCB-EK-002059        PIC X(10).
                   15  SCB-EV-002059        PIC 9(06) COMP.
               10  SCB-A-002060             PIC 9(09) COMP VALUE 2060.
               10  SCB-B-002061             PIC S9(07)V99 COMP-3 VALUE +0002061.
               10  SCB-C-002062             PIC X(24) VALUE 'FIELD-002062-ALPHA'.
               10  SCB-D-002063             PIC 9(04) VALUE 4441.
               10  SCB-DESC-002063          PIC X(18) VALUE 'DESC-026819'.
               10  SCB-E-002064             OCCURS 2 TIMES.
                   15  SCB-EK-002064        PIC X(10).
                   15  SCB-EV-002064        PIC 9(06) COMP.
               10  SCB-A-002065             PIC 9(09) COMP VALUE 2065.
               10  SCB-B-002066             PIC S9(07)V99 COMP-3 VALUE +0002066.
               10  SCB-C-002067             PIC X(24) VALUE 'FIELD-002067-ALPHA'.
               10  SCB-D-002068             PIC 9(04) VALUE 4476.
               10  SCB-DESC-002068          PIC X(18) VALUE 'DESC-026884'.
               10  SCB-E-002069             OCCURS 3 TIMES.
                   15  SCB-EK-002069        PIC X(10).
                   15  SCB-EV-002069        PIC 9(06) COMP.
               10  SCB-A-002070             PIC 9(09) COMP VALUE 2070.
               10  SCB-B-002071             PIC S9(07)V99 COMP-3 VALUE +0002071.
           05  SCB-GRP-0189.
               10  SCB-C-002072             PIC X(24) VALUE 'FIELD-002072-ALPHA'.
               10  SCB-D-002073             PIC 9(04) VALUE 4511.
               10  SCB-DESC-002073          PIC X(18) VALUE 'DESC-026949'.
               10  SCB-E-002074             OCCURS 4 TIMES.
                   15  SCB-EK-002074        PIC X(10).
                   15  SCB-EV-002074        PIC 9(06) COMP.
               10  SCB-A-002075             PIC 9(09) COMP VALUE 2075.
               10  SCB-B-002076             PIC S9(07)V99 COMP-3 VALUE +0002076.
               10  SCB-C-002077             PIC X(24) VALUE 'FIELD-002077-ALPHA'.
               10  SCB-D-002078             PIC 9(04) VALUE 4546.
               10  SCB-DESC-002078          PIC X(18) VALUE 'DESC-027014'.
               10  SCB-E-002079             OCCURS 5 TIMES.
                   15  SCB-EK-002079        PIC X(10).
                   15  SCB-EV-002079        PIC 9(06) COMP.
           05  SCB-GRP-0190.
               10  SCB-A-002080             PIC 9(09) COMP VALUE 2080.
               10  SCB-B-002081             PIC S9(07)V99 COMP-3 VALUE +0002081.
               10  SCB-C-002082             PIC X(24) VALUE 'FIELD-002082-ALPHA'.
               10  SCB-D-002083             PIC 9(04) VALUE 4581.
               10  SCB-DESC-002083          PIC X(18) VALUE 'DESC-027079'.
               10  SCB-E-002084             OCCURS 2 TIMES.
                   15  SCB-EK-002084        PIC X(10).
                   15  SCB-EV-002084        PIC 9(06) COMP.
               10  SCB-A-002085             PIC 9(09) COMP VALUE 2085.
               10  SCB-B-002086             PIC S9(07)V99 COMP-3 VALUE +0002086.
               10  SCB-C-002087             PIC X(24) VALUE 'FIELD-002087-ALPHA'.
               10  SCB-D-002088             PIC 9(04) VALUE 4616.
               10  SCB-DESC-002088          PIC X(18) VALUE 'DESC-027144'.
           05  SCB-GRP-0191.
               10  SCB-E-002089             OCCURS 3 TIMES.
                   15  SCB-EK-002089        PIC X(10).
                   15  SCB-EV-002089        PIC 9(06) COMP.
               10  SCB-A-002090             PIC 9(09) COMP VALUE 2090.
               10  SCB-B-002091             PIC S9(07)V99 COMP-3 VALUE +0002091.
               10  SCB-C-002092             PIC X(24) VALUE 'FIELD-002092-ALPHA'.
               10  SCB-D-002093             PIC 9(04) VALUE 4651.
               10  SCB-DESC-002093          PIC X(18) VALUE 'DESC-027209'.
               10  SCB-E-002094             OCCURS 4 TIMES.
                   15  SCB-EK-002094        PIC X(10).
                   15  SCB-EV-002094        PIC 9(06) COMP.
               10  SCB-A-002095             PIC 9(09) COMP VALUE 2095.
               10  SCB-B-002096             PIC S9(07)V99 COMP-3 VALUE +0002096.
               10  SCB-C-002097             PIC X(24) VALUE 'FIELD-002097-ALPHA'.
               10  SCB-D-002098             PIC 9(04) VALUE 4686.
               10  SCB-DESC-002098          PIC X(18) VALUE 'DESC-027274'.
           05  SCB-GRP-0192.
               10  SCB-E-002099             OCCURS 5 TIMES.
                   15  SCB-EK-002099        PIC X(10).
                   15  SCB-EV-002099        PIC 9(06) COMP.
               10  SCB-A-002100             PIC 9(09) COMP VALUE 2100.
               10  SCB-B-002101             PIC S9(07)V99 COMP-3 VALUE +0002101.
               10  SCB-C-002102             PIC X(24) VALUE 'FIELD-002102-ALPHA'.
               10  SCB-D-002103             PIC 9(04) VALUE 4721.
               10  SCB-DESC-002103          PIC X(18) VALUE 'DESC-027339'.
               10  SCB-E-002104             OCCURS 2 TIMES.
                   15  SCB-EK-002104        PIC X(10).
                   15  SCB-EV-002104        PIC 9(06) COMP.
               10  SCB-A-002105             PIC 9(09) COMP VALUE 2105.
               10  SCB-B-002106             PIC S9(07)V99 COMP-3 VALUE +0002106.
               10  SCB-C-002107             PIC X(24) VALUE 'FIELD-002107-ALPHA'.
               10  SCB-D-002108             PIC 9(04) VALUE 4756.
               10  SCB-DESC-002108          PIC X(18) VALUE 'DESC-027404'.
               10  SCB-E-002109             OCCURS 3 TIMES.
                   15  SCB-EK-002109        PIC X(10).
                   15  SCB-EV-002109        PIC 9(06) COMP.
           05  SCB-ALT-0032 REDEFINES SCB-GRP-0192.
               10  SCB-ALT-FLAG-0032      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0032   PIC X(64).
           05  SCB-GRP-0193.
               10  SCB-A-002110             PIC 9(09) COMP VALUE 2110.
               10  SCB-B-002111             PIC S9(07)V99 COMP-3 VALUE +0002111.
               10  SCB-C-002112             PIC X(24) VALUE 'FIELD-002112-ALPHA'.
               10  SCB-D-002113             PIC 9(04) VALUE 4791.
               10  SCB-DESC-002113          PIC X(18) VALUE 'DESC-027469'.
               10  SCB-E-002114             OCCURS 4 TIMES.
                   15  SCB-EK-002114        PIC X(10).
                   15  SCB-EV-002114        PIC 9(06) COMP.
               10  SCB-A-002115             PIC 9(09) COMP VALUE 2115.
               10  SCB-B-002116             PIC S9(07)V99 COMP-3 VALUE +0002116.
               10  SCB-C-002117             PIC X(24) VALUE 'FIELD-002117-ALPHA'.
               10  SCB-D-002118             PIC 9(04) VALUE 4826.
               10  SCB-DESC-002118          PIC X(18) VALUE 'DESC-027534'.
               10  SCB-E-002119             OCCURS 5 TIMES.
                   15  SCB-EK-002119        PIC X(10).
                   15  SCB-EV-002119        PIC 9(06) COMP.
               10  SCB-A-002120             PIC 9(09) COMP VALUE 2120.
               10  SCB-B-002121             PIC S9(07)V99 COMP-3 VALUE +0002121.
           05  SCB-GRP-0194.
               10  SCB-C-002122             PIC X(24) VALUE 'FIELD-002122-ALPHA'.
               10  SCB-D-002123             PIC 9(04) VALUE 4861.
               10  SCB-DESC-002123          PIC X(18) VALUE 'DESC-027599'.
               10  SCB-E-002124             OCCURS 2 TIMES.
                   15  SCB-EK-002124        PIC X(10).
                   15  SCB-EV-002124        PIC 9(06) COMP.
               10  SCB-A-002125             PIC 9(09) COMP VALUE 2125.
               10  SCB-B-002126             PIC S9(07)V99 COMP-3 VALUE +0002126.
               10  SCB-C-002127             PIC X(24) VALUE 'FIELD-002127-ALPHA'.
               10  SCB-D-002128             PIC 9(04) VALUE 4896.
               10  SCB-DESC-002128          PIC X(18) VALUE 'DESC-027664'.
               10  SCB-E-002129             OCCURS 3 TIMES.
                   15  SCB-EK-002129        PIC X(10).
                   15  SCB-EV-002129        PIC 9(06) COMP.
               10  SCB-A-002130             PIC 9(09) COMP VALUE 2130.
               10  SCB-B-002131             PIC S9(07)V99 COMP-3 VALUE +0002131.
               10  SCB-C-002132             PIC X(24) VALUE 'FIELD-002132-ALPHA'.
               10  SCB-D-002133             PIC 9(04) VALUE 4931.
               10  SCB-DESC-002133          PIC X(18) VALUE 'DESC-027729'.
               10  SCB-E-002134             OCCURS 4 TIMES.
                   15  SCB-EK-002134        PIC X(10).
                   15  SCB-EV-002134        PIC 9(06) COMP.
           05  SCB-GRP-0195.
               10  SCB-A-002135             PIC 9(09) COMP VALUE 2135.
               10  SCB-B-002136             PIC S9(07)V99 COMP-3 VALUE +0002136.
               10  SCB-C-002137             PIC X(24) VALUE 'FIELD-002137-ALPHA'.
               10  SCB-D-002138             PIC 9(04) VALUE 4966.
               10  SCB-DESC-002138          PIC X(18) VALUE 'DESC-027794'.
               10  SCB-E-002139             OCCURS 5 TIMES.
                   15  SCB-EK-002139        PIC X(10).
                   15  SCB-EV-002139        PIC 9(06) COMP.
               10  SCB-A-002140             PIC 9(09) COMP VALUE 2140.
               10  SCB-B-002141             PIC S9(07)V99 COMP-3 VALUE +0002141.
               10  SCB-C-002142             PIC X(24) VALUE 'FIELD-002142-ALPHA'.
               10  SCB-D-002143             PIC 9(04) VALUE 5001.
               10  SCB-DESC-002143          PIC X(18) VALUE 'DESC-027859'.
               10  SCB-E-002144             OCCURS 2 TIMES.
                   15  SCB-EK-002144        PIC X(10).
                   15  SCB-EV-002144        PIC 9(06) COMP.
               10  SCB-A-002145             PIC 9(09) COMP VALUE 2145.
               10  SCB-B-002146             PIC S9(07)V99 COMP-3 VALUE +0002146.
               10  SCB-C-002147             PIC X(24) VALUE 'FIELD-002147-ALPHA'.
               10  SCB-D-002148             PIC 9(04) VALUE 5036.
               10  SCB-DESC-002148          PIC X(18) VALUE 'DESC-027924'.
           05  SCB-GRP-0196.
               10  SCB-E-002149             OCCURS 3 TIMES.
                   15  SCB-EK-002149        PIC X(10).
                   15  SCB-EV-002149        PIC 9(06) COMP.
               10  SCB-A-002150             PIC 9(09) COMP VALUE 2150.
               10  SCB-B-002151             PIC S9(07)V99 COMP-3 VALUE +0002151.
               10  SCB-C-002152             PIC X(24) VALUE 'FIELD-002152-ALPHA'.
               10  SCB-D-002153             PIC 9(04) VALUE 5071.
               10  SCB-DESC-002153          PIC X(18) VALUE 'DESC-027989'.
               10  SCB-E-002154             OCCURS 4 TIMES.
                   15  SCB-EK-002154        PIC X(10).
                   15  SCB-EV-002154        PIC 9(06) COMP.
               10  SCB-A-002155             PIC 9(09) COMP VALUE 2155.
               10  SCB-B-002156             PIC S9(07)V99 COMP-3 VALUE +0002156.
           05  SCB-GRP-0197.
               10  SCB-C-002157             PIC X(24) VALUE 'FIELD-002157-ALPHA'.
               10  SCB-D-002158             PIC 9(04) VALUE 5106.
               10  SCB-DESC-002158          PIC X(18) VALUE 'DESC-028054'.
               10  SCB-E-002159             OCCURS 5 TIMES.
                   15  SCB-EK-002159        PIC X(10).
                   15  SCB-EV-002159        PIC 9(06) COMP.
               10  SCB-A-002160             PIC 9(09) COMP VALUE 2160.
               10  SCB-B-002161             PIC S9(07)V99 COMP-3 VALUE +0002161.
               10  SCB-C-002162             PIC X(24) VALUE 'FIELD-002162-ALPHA'.
               10  SCB-D-002163             PIC 9(04) VALUE 5141.
               10  SCB-DESC-002163          PIC X(18) VALUE 'DESC-028119'.
               10  SCB-E-002164             OCCURS 2 TIMES.
                   15  SCB-EK-002164        PIC X(10).
                   15  SCB-EV-002164        PIC 9(06) COMP.
               10  SCB-A-002165             PIC 9(09) COMP VALUE 2165.
           05  SCB-GRP-0198.
               10  SCB-B-002166             PIC S9(07)V99 COMP-3 VALUE +0002166.
               10  SCB-C-002167             PIC X(24) VALUE 'FIELD-002167-ALPHA'.
               10  SCB-D-002168             PIC 9(04) VALUE 5176.
               10  SCB-DESC-002168          PIC X(18) VALUE 'DESC-028184'.
               10  SCB-E-002169             OCCURS 3 TIMES.
                   15  SCB-EK-002169        PIC X(10).
                   15  SCB-EV-002169        PIC 9(06) COMP.
               10  SCB-A-002170             PIC 9(09) COMP VALUE 2170.
               10  SCB-B-002171             PIC S9(07)V99 COMP-3 VALUE +0002171.
               10  SCB-C-002172             PIC X(24) VALUE 'FIELD-002172-ALPHA'.
               10  SCB-D-002173             PIC 9(04) VALUE 5211.
               10  SCB-DESC-002173          PIC X(18) VALUE 'DESC-028249'.
               10  SCB-E-002174             OCCURS 4 TIMES.
                   15  SCB-EK-002174        PIC X(10).
                   15  SCB-EV-002174        PIC 9(06) COMP.
               10  SCB-A-002175             PIC 9(09) COMP VALUE 2175.
           05  SCB-ALT-0033 REDEFINES SCB-GRP-0198.
               10  SCB-ALT-FLAG-0033      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0033   PIC X(64).
           05  SCB-GRP-0199.
               10  SCB-B-002176             PIC S9(07)V99 COMP-3 VALUE +0002176.
               10  SCB-C-002177             PIC X(24) VALUE 'FIELD-002177-ALPHA'.
               10  SCB-D-002178             PIC 9(04) VALUE 5246.
               10  SCB-DESC-002178          PIC X(18) VALUE 'DESC-028314'.
               10  SCB-E-002179             OCCURS 5 TIMES.
                   15  SCB-EK-002179        PIC X(10).
                   15  SCB-EV-002179        PIC 9(06) COMP.
               10  SCB-A-002180             PIC 9(09) COMP VALUE 2180.
               10  SCB-B-002181             PIC S9(07)V99 COMP-3 VALUE +0002181.
               10  SCB-C-002182             PIC X(24) VALUE 'FIELD-002182-ALPHA'.
               10  SCB-D-002183             PIC 9(04) VALUE 5281.
               10  SCB-DESC-002183          PIC X(18) VALUE 'DESC-028379'.
               10  SCB-E-002184             OCCURS 2 TIMES.
                   15  SCB-EK-002184        PIC X(10).
                   15  SCB-EV-002184        PIC 9(06) COMP.
               10  SCB-A-002185             PIC 9(09) COMP VALUE 2185.
               10  SCB-B-002186             PIC S9(07)V99 COMP-3 VALUE +0002186.
           05  SCB-GRP-0200.
               10  SCB-C-002187             PIC X(24) VALUE 'FIELD-002187-ALPHA'.
               10  SCB-D-002188             PIC 9(04) VALUE 5316.
               10  SCB-DESC-002188          PIC X(18) VALUE 'DESC-028444'.
               10  SCB-E-002189             OCCURS 3 TIMES.
                   15  SCB-EK-002189        PIC X(10).
                   15  SCB-EV-002189        PIC 9(06) COMP.
               10  SCB-A-002190             PIC 9(09) COMP VALUE 2190.
               10  SCB-B-002191             PIC S9(07)V99 COMP-3 VALUE +0002191.
               10  SCB-C-002192             PIC X(24) VALUE 'FIELD-002192-ALPHA'.
               10  SCB-D-002193             PIC 9(04) VALUE 5351.
               10  SCB-DESC-002193          PIC X(18) VALUE 'DESC-028509'.
               10  SCB-E-002194             OCCURS 4 TIMES.
                   15  SCB-EK-002194        PIC X(10).
                   15  SCB-EV-002194        PIC 9(06) COMP.
               10  SCB-A-002195             PIC 9(09) COMP VALUE 2195.
               10  SCB-B-002196             PIC S9(07)V99 COMP-3 VALUE +0002196.
               10  SCB-C-002197             PIC X(24) VALUE 'FIELD-002197-ALPHA'.
               10  SCB-D-002198             PIC 9(04) VALUE 5386.
               10  SCB-DESC-002198          PIC X(18) VALUE 'DESC-028574'.
           05  SCB-GRP-0201.
               10  SCB-E-002199             OCCURS 5 TIMES.
                   15  SCB-EK-002199        PIC X(10).
                   15  SCB-EV-002199        PIC 9(06) COMP.
               10  SCB-A-002200             PIC 9(09) COMP VALUE 2200.
               10  SCB-B-002201             PIC S9(07)V99 COMP-3 VALUE +0002201.
               10  SCB-C-002202             PIC X(24) VALUE 'FIELD-002202-ALPHA'.
               10  SCB-D-002203             PIC 9(04) VALUE 5421.
               10  SCB-DESC-002203          PIC X(18) VALUE 'DESC-028639'.
               10  SCB-E-002204             OCCURS 2 TIMES.
                   15  SCB-EK-002204        PIC X(10).
                   15  SCB-EV-002204        PIC 9(06) COMP.
               10  SCB-A-002205             PIC 9(09) COMP VALUE 2205.
               10  SCB-B-002206             PIC S9(07)V99 COMP-3 VALUE +0002206.
               10  SCB-C-002207             PIC X(24) VALUE 'FIELD-002207-ALPHA'.
               10  SCB-D-002208             PIC 9(04) VALUE 5456.
               10  SCB-DESC-002208          PIC X(18) VALUE 'DESC-028704'.
               10  SCB-E-002209             OCCURS 3 TIMES.
                   15  SCB-EK-002209        PIC X(10).
                   15  SCB-EV-002209        PIC 9(06) COMP.
               10  SCB-A-002210             PIC 9(09) COMP VALUE 2210.
               10  SCB-B-002211             PIC S9(07)V99 COMP-3 VALUE +0002211.
           05  SCB-GRP-0202.
               10  SCB-C-002212             PIC X(24) VALUE 'FIELD-002212-ALPHA'.
               10  SCB-D-002213             PIC 9(04) VALUE 5491.
               10  SCB-DESC-002213          PIC X(18) VALUE 'DESC-028769'.
               10  SCB-E-002214             OCCURS 4 TIMES.
                   15  SCB-EK-002214        PIC X(10).
                   15  SCB-EV-002214        PIC 9(06) COMP.
               10  SCB-A-002215             PIC 9(09) COMP VALUE 2215.
               10  SCB-B-002216             PIC S9(07)V99 COMP-3 VALUE +0002216.
               10  SCB-C-002217             PIC X(24) VALUE 'FIELD-002217-ALPHA'.
               10  SCB-D-002218             PIC 9(04) VALUE 5526.
               10  SCB-DESC-002218          PIC X(18) VALUE 'DESC-028834'.
               10  SCB-E-002219             OCCURS 5 TIMES.
                   15  SCB-EK-002219        PIC X(10).
                   15  SCB-EV-002219        PIC 9(06) COMP.
               10  SCB-A-002220             PIC 9(09) COMP VALUE 2220.
               10  SCB-B-002221             PIC S9(07)V99 COMP-3 VALUE +0002221.
               10  SCB-C-002222             PIC X(24) VALUE 'FIELD-002222-ALPHA'.
               10  SCB-D-002223             PIC 9(04) VALUE 5561.
               10  SCB-DESC-002223          PIC X(18) VALUE 'DESC-028899'.
               10  SCB-E-002224             OCCURS 2 TIMES.
                   15  SCB-EK-002224        PIC X(10).
                   15  SCB-EV-002224        PIC 9(06) COMP.
               10  SCB-A-002225             PIC 9(09) COMP VALUE 2225.
           05  SCB-GRP-0203.
               10  SCB-B-002226             PIC S9(07)V99 COMP-3 VALUE +0002226.
               10  SCB-C-002227             PIC X(24) VALUE 'FIELD-002227-ALPHA'.
               10  SCB-D-002228             PIC 9(04) VALUE 5596.
               10  SCB-DESC-002228          PIC X(18) VALUE 'DESC-028964'.
               10  SCB-E-002229             OCCURS 3 TIMES.
                   15  SCB-EK-002229        PIC X(10).
                   15  SCB-EV-002229        PIC 9(06) COMP.
               10  SCB-A-002230             PIC 9(09) COMP VALUE 2230.
               10  SCB-B-002231             PIC S9(07)V99 COMP-3 VALUE +0002231.
               10  SCB-C-002232             PIC X(24) VALUE 'FIELD-002232-ALPHA'.
               10  SCB-D-002233             PIC 9(04) VALUE 5631.
               10  SCB-DESC-002233          PIC X(18) VALUE 'DESC-029029'.
           05  SCB-GRP-0204.
               10  SCB-E-002234             OCCURS 4 TIMES.
                   15  SCB-EK-002234        PIC X(10).
                   15  SCB-EV-002234        PIC 9(06) COMP.
               10  SCB-A-002235             PIC 9(09) COMP VALUE 2235.
               10  SCB-B-002236             PIC S9(07)V99 COMP-3 VALUE +0002236.
               10  SCB-C-002237             PIC X(24) VALUE 'FIELD-002237-ALPHA'.
               10  SCB-D-002238             PIC 9(04) VALUE 5666.
               10  SCB-DESC-002238          PIC X(18) VALUE 'DESC-029094'.
               10  SCB-E-002239             OCCURS 5 TIMES.
                   15  SCB-EK-002239        PIC X(10).
                   15  SCB-EV-002239        PIC 9(06) COMP.
               10  SCB-A-002240             PIC 9(09) COMP VALUE 2240.
               10  SCB-B-002241             PIC S9(07)V99 COMP-3 VALUE +0002241.
               10  SCB-C-002242             PIC X(24) VALUE 'FIELD-002242-ALPHA'.
           05  SCB-ALT-0034 REDEFINES SCB-GRP-0204.
               10  SCB-ALT-FLAG-0034      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0034   PIC X(64).
           05  SCB-GRP-0205.
               10  SCB-D-002243             PIC 9(04) VALUE 5701.
               10  SCB-DESC-002243          PIC X(18) VALUE 'DESC-029159'.
               10  SCB-E-002244             OCCURS 2 TIMES.
                   15  SCB-EK-002244        PIC X(10).
                   15  SCB-EV-002244        PIC 9(06) COMP.
               10  SCB-A-002245             PIC 9(09) COMP VALUE 2245.
               10  SCB-B-002246             PIC S9(07)V99 COMP-3 VALUE +0002246.
               10  SCB-C-002247             PIC X(24) VALUE 'FIELD-002247-ALPHA'.
               10  SCB-D-002248             PIC 9(04) VALUE 5736.
               10  SCB-DESC-002248          PIC X(18) VALUE 'DESC-029224'.
               10  SCB-E-002249             OCCURS 3 TIMES.
                   15  SCB-EK-002249        PIC X(10).
                   15  SCB-EV-002249        PIC 9(06) COMP.
               10  SCB-A-002250             PIC 9(09) COMP VALUE 2250.
               10  SCB-B-002251             PIC S9(07)V99 COMP-3 VALUE +0002251.
               10  SCB-C-002252             PIC X(24) VALUE 'FIELD-002252-ALPHA'.
           05  SCB-GRP-0206.
               10  SCB-D-002253             PIC 9(04) VALUE 5771.
               10  SCB-DESC-002253          PIC X(18) VALUE 'DESC-029289'.
               10  SCB-E-002254             OCCURS 4 TIMES.
                   15  SCB-EK-002254        PIC X(10).
                   15  SCB-EV-002254        PIC 9(06) COMP.
               10  SCB-A-002255             PIC 9(09) COMP VALUE 2255.
               10  SCB-B-002256             PIC S9(07)V99 COMP-3 VALUE +0002256.
               10  SCB-C-002257             PIC X(24) VALUE 'FIELD-002257-ALPHA'.
               10  SCB-D-002258             PIC 9(04) VALUE 5806.
               10  SCB-DESC-002258          PIC X(18) VALUE 'DESC-029354'.
               10  SCB-E-002259             OCCURS 5 TIMES.
                   15  SCB-EK-002259        PIC X(10).
                   15  SCB-EV-002259        PIC 9(06) COMP.
               10  SCB-A-002260             PIC 9(09) COMP VALUE 2260.
               10  SCB-B-002261             PIC S9(07)V99 COMP-3 VALUE +0002261.
               10  SCB-C-002262             PIC X(24) VALUE 'FIELD-002262-ALPHA'.
               10  SCB-D-002263             PIC 9(04) VALUE 5841.
               10  SCB-DESC-002263          PIC X(18) VALUE 'DESC-029419'.
           05  SCB-GRP-0207.
               10  SCB-E-002264             OCCURS 2 TIMES.
                   15  SCB-EK-002264        PIC X(10).
                   15  SCB-EV-002264        PIC 9(06) COMP.
               10  SCB-A-002265             PIC 9(09) COMP VALUE 2265.
               10  SCB-B-002266             PIC S9(07)V99 COMP-3 VALUE +0002266.
               10  SCB-C-002267             PIC X(24) VALUE 'FIELD-002267-ALPHA'.
               10  SCB-D-002268             PIC 9(04) VALUE 5876.
               10  SCB-DESC-002268          PIC X(18) VALUE 'DESC-029484'.
               10  SCB-E-002269             OCCURS 3 TIMES.
                   15  SCB-EK-002269        PIC X(10).
                   15  SCB-EV-002269        PIC 9(06) COMP.
               10  SCB-A-002270             PIC 9(09) COMP VALUE 2270.
               10  SCB-B-002271             PIC S9(07)V99 COMP-3 VALUE +0002271.
               10  SCB-C-002272             PIC X(24) VALUE 'FIELD-002272-ALPHA'.
               10  SCB-D-002273             PIC 9(04) VALUE 5911.
               10  SCB-DESC-002273          PIC X(18) VALUE 'DESC-029549'.
               10  SCB-E-002274             OCCURS 4 TIMES.
                   15  SCB-EK-002274        PIC X(10).
                   15  SCB-EV-002274        PIC 9(06) COMP.
               10  SCB-A-002275             PIC 9(09) COMP VALUE 2275.
           05  SCB-GRP-0208.
               10  SCB-B-002276             PIC S9(07)V99 COMP-3 VALUE +0002276.
               10  SCB-C-002277             PIC X(24) VALUE 'FIELD-002277-ALPHA'.
               10  SCB-D-002278             PIC 9(04) VALUE 5946.
               10  SCB-DESC-002278          PIC X(18) VALUE 'DESC-029614'.
               10  SCB-E-002279             OCCURS 5 TIMES.
                   15  SCB-EK-002279        PIC X(10).
                   15  SCB-EV-002279        PIC 9(06) COMP.
               10  SCB-A-002280             PIC 9(09) COMP VALUE 2280.
               10  SCB-B-002281             PIC S9(07)V99 COMP-3 VALUE +0002281.
               10  SCB-C-002282             PIC X(24) VALUE 'FIELD-002282-ALPHA'.
               10  SCB-D-002283             PIC 9(04) VALUE 5981.
               10  SCB-DESC-002283          PIC X(18) VALUE 'DESC-029679'.
               10  SCB-E-002284             OCCURS 2 TIMES.
                   15  SCB-EK-002284        PIC X(10).
                   15  SCB-EV-002284        PIC 9(06) COMP.
               10  SCB-A-002285             PIC 9(09) COMP VALUE 2285.
               10  SCB-B-002286             PIC S9(07)V99 COMP-3 VALUE +0002286.
               10  SCB-C-002287             PIC X(24) VALUE 'FIELD-002287-ALPHA'.
               10  SCB-D-002288             PIC 9(04) VALUE 6016.
               10  SCB-DESC-002288          PIC X(18) VALUE 'DESC-029744'.
           05  SCB-GRP-0209.
               10  SCB-E-002289             OCCURS 3 TIMES.
                   15  SCB-EK-002289        PIC X(10).
                   15  SCB-EV-002289        PIC 9(06) COMP.
               10  SCB-A-002290             PIC 9(09) COMP VALUE 2290.
               10  SCB-B-002291             PIC S9(07)V99 COMP-3 VALUE +0002291.
               10  SCB-C-002292             PIC X(24) VALUE 'FIELD-002292-ALPHA'.
               10  SCB-D-002293             PIC 9(04) VALUE 6051.
               10  SCB-DESC-002293          PIC X(18) VALUE 'DESC-029809'.
               10  SCB-E-002294             OCCURS 4 TIMES.
                   15  SCB-EK-002294        PIC X(10).
                   15  SCB-EV-002294        PIC 9(06) COMP.
               10  SCB-A-002295             PIC 9(09) COMP VALUE 2295.
               10  SCB-B-002296             PIC S9(07)V99 COMP-3 VALUE +0002296.
               10  SCB-C-002297             PIC X(24) VALUE 'FIELD-002297-ALPHA'.
               10  SCB-D-002298             PIC 9(04) VALUE 6086.
               10  SCB-DESC-002298          PIC X(18) VALUE 'DESC-029874'.
               10  SCB-E-002299             OCCURS 5 TIMES.
                   15  SCB-EK-002299        PIC X(10).
                   15  SCB-EV-002299        PIC 9(06) COMP.
               10  SCB-A-002300             PIC 9(09) COMP VALUE 2300.
               10  SCB-B-002301             PIC S9(07)V99 COMP-3 VALUE +0002301.
               10  SCB-C-002302             PIC X(24) VALUE 'FIELD-002302-ALPHA'.
           05  SCB-GRP-0210.
               10  SCB-D-002303             PIC 9(04) VALUE 6121.
               10  SCB-DESC-002303          PIC X(18) VALUE 'DESC-029939'.
               10  SCB-E-002304             OCCURS 2 TIMES.
                   15  SCB-EK-002304        PIC X(10).
                   15  SCB-EV-002304        PIC 9(06) COMP.
               10  SCB-A-002305             PIC 9(09) COMP VALUE 2305.
               10  SCB-B-002306             PIC S9(07)V99 COMP-3 VALUE +0002306.
               10  SCB-C-002307             PIC X(24) VALUE 'FIELD-002307-ALPHA'.
               10  SCB-D-002308             PIC 9(04) VALUE 6156.
               10  SCB-DESC-002308          PIC X(18) VALUE 'DESC-030004'.
               10  SCB-E-002309             OCCURS 3 TIMES.
                   15  SCB-EK-002309        PIC X(10).
                   15  SCB-EV-002309        PIC 9(06) COMP.
               10  SCB-A-002310             PIC 9(09) COMP VALUE 2310.
           05  SCB-ALT-0035 REDEFINES SCB-GRP-0210.
               10  SCB-ALT-FLAG-0035      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0035   PIC X(64).
           05  SCB-GRP-0211.
               10  SCB-B-002311             PIC S9(07)V99 COMP-3 VALUE +0002311.
               10  SCB-C-002312             PIC X(24) VALUE 'FIELD-002312-ALPHA'.
               10  SCB-D-002313             PIC 9(04) VALUE 6191.
               10  SCB-DESC-002313          PIC X(18) VALUE 'DESC-030069'.
               10  SCB-E-002314             OCCURS 4 TIMES.
                   15  SCB-EK-002314        PIC X(10).
                   15  SCB-EV-002314        PIC 9(06) COMP.
               10  SCB-A-002315             PIC 9(09) COMP VALUE 2315.
               10  SCB-B-002316             PIC S9(07)V99 COMP-3 VALUE +0002316.
               10  SCB-C-002317             PIC X(24) VALUE 'FIELD-002317-ALPHA'.
               10  SCB-D-002318             PIC 9(04) VALUE 6226.
               10  SCB-DESC-002318          PIC X(18) VALUE 'DESC-030134'.
               10  SCB-E-002319             OCCURS 5 TIMES.
                   15  SCB-EK-002319        PIC X(10).
                   15  SCB-EV-002319        PIC 9(06) COMP.
           05  SCB-GRP-0212.
               10  SCB-A-002320             PIC 9(09) COMP VALUE 2320.
               10  SCB-B-002321             PIC S9(07)V99 COMP-3 VALUE +0002321.
               10  SCB-C-002322             PIC X(24) VALUE 'FIELD-002322-ALPHA'.
               10  SCB-D-002323             PIC 9(04) VALUE 6261.
               10  SCB-DESC-002323          PIC X(18) VALUE 'DESC-030199'.
               10  SCB-E-002324             OCCURS 2 TIMES.
                   15  SCB-EK-002324        PIC X(10).
                   15  SCB-EV-002324        PIC 9(06) COMP.
               10  SCB-A-002325             PIC 9(09) COMP VALUE 2325.
               10  SCB-B-002326             PIC S9(07)V99 COMP-3 VALUE +0002326.
               10  SCB-C-002327             PIC X(24) VALUE 'FIELD-002327-ALPHA'.
               10  SCB-D-002328             PIC 9(04) VALUE 6296.
               10  SCB-DESC-002328          PIC X(18) VALUE 'DESC-030264'.
               10  SCB-E-002329             OCCURS 3 TIMES.
                   15  SCB-EK-002329        PIC X(10).
                   15  SCB-EV-002329        PIC 9(06) COMP.
           05  SCB-GRP-0213.
               10  SCB-A-002330             PIC 9(09) COMP VALUE 2330.
               10  SCB-B-002331             PIC S9(07)V99 COMP-3 VALUE +0002331.
               10  SCB-C-002332             PIC X(24) VALUE 'FIELD-002332-ALPHA'.
               10  SCB-D-002333             PIC 9(04) VALUE 6331.
               10  SCB-DESC-002333          PIC X(18) VALUE 'DESC-030329'.
               10  SCB-E-002334             OCCURS 4 TIMES.
                   15  SCB-EK-002334        PIC X(10).
                   15  SCB-EV-002334        PIC 9(06) COMP.
               10  SCB-A-002335             PIC 9(09) COMP VALUE 2335.
               10  SCB-B-002336             PIC S9(07)V99 COMP-3 VALUE +0002336.
               10  SCB-C-002337             PIC X(24) VALUE 'FIELD-002337-ALPHA'.
               10  SCB-D-002338             PIC 9(04) VALUE 6366.
               10  SCB-DESC-002338          PIC X(18) VALUE 'DESC-030394'.
               10  SCB-E-002339             OCCURS 5 TIMES.
                   15  SCB-EK-002339        PIC X(10).
                   15  SCB-EV-002339        PIC 9(06) COMP.
               10  SCB-A-002340             PIC 9(09) COMP VALUE 2340.
           05  SCB-GRP-0214.
               10  SCB-B-002341             PIC S9(07)V99 COMP-3 VALUE +0002341.
               10  SCB-C-002342             PIC X(24) VALUE 'FIELD-002342-ALPHA'.
               10  SCB-D-002343             PIC 9(04) VALUE 6401.
               10  SCB-DESC-002343          PIC X(18) VALUE 'DESC-030459'.
               10  SCB-E-002344             OCCURS 2 TIMES.
                   15  SCB-EK-002344        PIC X(10).
                   15  SCB-EV-002344        PIC 9(06) COMP.
               10  SCB-A-002345             PIC 9(09) COMP VALUE 2345.
               10  SCB-B-002346             PIC S9(07)V99 COMP-3 VALUE +0002346.
               10  SCB-C-002347             PIC X(24) VALUE 'FIELD-002347-ALPHA'.
               10  SCB-D-002348             PIC 9(04) VALUE 6436.
               10  SCB-DESC-002348          PIC X(18) VALUE 'DESC-030524'.
               10  SCB-E-002349             OCCURS 3 TIMES.
                   15  SCB-EK-002349        PIC X(10).
                   15  SCB-EV-002349        PIC 9(06) COMP.
               10  SCB-A-002350             PIC 9(09) COMP VALUE 2350.
               10  SCB-B-002351             PIC S9(07)V99 COMP-3 VALUE +0002351.
               10  SCB-C-002352             PIC X(24) VALUE 'FIELD-002352-ALPHA'.
           05  SCB-GRP-0215.
               10  SCB-D-002353             PIC 9(04) VALUE 6471.
               10  SCB-DESC-002353          PIC X(18) VALUE 'DESC-030589'.
               10  SCB-E-002354             OCCURS 4 TIMES.
                   15  SCB-EK-002354        PIC X(10).
                   15  SCB-EV-002354        PIC 9(06) COMP.
               10  SCB-A-002355             PIC 9(09) COMP VALUE 2355.
               10  SCB-B-002356             PIC S9(07)V99 COMP-3 VALUE +0002356.
               10  SCB-C-002357             PIC X(24) VALUE 'FIELD-002357-ALPHA'.
               10  SCB-D-002358             PIC 9(04) VALUE 6506.
               10  SCB-DESC-002358          PIC X(18) VALUE 'DESC-030654'.
               10  SCB-E-002359             OCCURS 5 TIMES.
                   15  SCB-EK-002359        PIC X(10).
                   15  SCB-EV-002359        PIC 9(06) COMP.
               10  SCB-A-002360             PIC 9(09) COMP VALUE 2360.
               10  SCB-B-002361             PIC S9(07)V99 COMP-3 VALUE +0002361.
               10  SCB-C-002362             PIC X(24) VALUE 'FIELD-002362-ALPHA'.
               10  SCB-D-002363             PIC 9(04) VALUE 6541.
               10  SCB-DESC-002363          PIC X(18) VALUE 'DESC-030719'.
               10  SCB-E-002364             OCCURS 2 TIMES.
                   15  SCB-EK-002364        PIC X(10).
                   15  SCB-EV-002364        PIC 9(06) COMP.
               10  SCB-A-002365             PIC 9(09) COMP VALUE 2365.
           05  SCB-GRP-0216.
               10  SCB-B-002366             PIC S9(07)V99 COMP-3 VALUE +0002366.
               10  SCB-C-002367             PIC X(24) VALUE 'FIELD-002367-ALPHA'.
               10  SCB-D-002368             PIC 9(04) VALUE 6576.
               10  SCB-DESC-002368          PIC X(18) VALUE 'DESC-030784'.
               10  SCB-E-002369             OCCURS 3 TIMES.
                   15  SCB-EK-002369        PIC X(10).
                   15  SCB-EV-002369        PIC 9(06) COMP.
               10  SCB-A-002370             PIC 9(09) COMP VALUE 2370.
               10  SCB-B-002371             PIC S9(07)V99 COMP-3 VALUE +0002371.
               10  SCB-C-002372             PIC X(24) VALUE 'FIELD-002372-ALPHA'.
               10  SCB-D-002373             PIC 9(04) VALUE 6611.
               10  SCB-DESC-002373          PIC X(18) VALUE 'DESC-030849'.
               10  SCB-E-002374             OCCURS 4 TIMES.
                   15  SCB-EK-002374        PIC X(10).
                   15  SCB-EV-002374        PIC 9(06) COMP.
               10  SCB-A-002375             PIC 9(09) COMP VALUE 2375.
               10  SCB-B-002376             PIC S9(07)V99 COMP-3 VALUE +0002376.
               10  SCB-C-002377             PIC X(24) VALUE 'FIELD-002377-ALPHA'.
               10  SCB-D-002378             PIC 9(04) VALUE 6646.
               10  SCB-DESC-002378          PIC X(18) VALUE 'DESC-030914'.
               10  SCB-E-002379             OCCURS 5 TIMES.
                   15  SCB-EK-002379        PIC X(10).
                   15  SCB-EV-002379        PIC 9(06) COMP.
           05  SCB-ALT-0036 REDEFINES SCB-GRP-0216.
               10  SCB-ALT-FLAG-0036      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0036   PIC X(64).
           05  SCB-GRP-0217.
               10  SCB-A-002380             PIC 9(09) COMP VALUE 2380.
               10  SCB-B-002381             PIC S9(07)V99 COMP-3 VALUE +0002381.
               10  SCB-C-002382             PIC X(24) VALUE 'FIELD-002382-ALPHA'.
               10  SCB-D-002383             PIC 9(04) VALUE 6681.
               10  SCB-DESC-002383          PIC X(18) VALUE 'DESC-030979'.
               10  SCB-E-002384             OCCURS 2 TIMES.
                   15  SCB-EK-002384        PIC X(10).
                   15  SCB-EV-002384        PIC 9(06) COMP.
               10  SCB-A-002385             PIC 9(09) COMP VALUE 2385.
               10  SCB-B-002386             PIC S9(07)V99 COMP-3 VALUE +0002386.
               10  SCB-C-002387             PIC X(24) VALUE 'FIELD-002387-ALPHA'.
           05  SCB-GRP-0218.
               10  SCB-D-002388             PIC 9(04) VALUE 6716.
               10  SCB-DESC-002388          PIC X(18) VALUE 'DESC-031044'.
               10  SCB-E-002389             OCCURS 3 TIMES.
                   15  SCB-EK-002389        PIC X(10).
                   15  SCB-EV-002389        PIC 9(06) COMP.
               10  SCB-A-002390             PIC 9(09) COMP VALUE 2390.
               10  SCB-B-002391             PIC S9(07)V99 COMP-3 VALUE +0002391.
               10  SCB-C-002392             PIC X(24) VALUE 'FIELD-002392-ALPHA'.
               10  SCB-D-002393             PIC 9(04) VALUE 6751.
               10  SCB-DESC-002393          PIC X(18) VALUE 'DESC-031109'.
               10  SCB-E-002394             OCCURS 4 TIMES.
                   15  SCB-EK-002394        PIC X(10).
                   15  SCB-EV-002394        PIC 9(06) COMP.
               10  SCB-A-002395             PIC 9(09) COMP VALUE 2395.
               10  SCB-B-002396             PIC S9(07)V99 COMP-3 VALUE +0002396.
           05  SCB-GRP-0219.
               10  SCB-C-002397             PIC X(24) VALUE 'FIELD-002397-ALPHA'.
               10  SCB-D-002398             PIC 9(04) VALUE 6786.
               10  SCB-DESC-002398          PIC X(18) VALUE 'DESC-031174'.
               10  SCB-E-002399             OCCURS 5 TIMES.
                   15  SCB-EK-002399        PIC X(10).
                   15  SCB-EV-002399        PIC 9(06) COMP.
               10  SCB-A-002400             PIC 9(09) COMP VALUE 2400.
               10  SCB-B-002401             PIC S9(07)V99 COMP-3 VALUE +0002401.
               10  SCB-C-002402             PIC X(24) VALUE 'FIELD-002402-ALPHA'.
               10  SCB-D-002403             PIC 9(04) VALUE 6821.
               10  SCB-DESC-002403          PIC X(18) VALUE 'DESC-031239'.
               10  SCB-E-002404             OCCURS 2 TIMES.
                   15  SCB-EK-002404        PIC X(10).
                   15  SCB-EV-002404        PIC 9(06) COMP.
               10  SCB-A-002405             PIC 9(09) COMP VALUE 2405.
               10  SCB-B-002406             PIC S9(07)V99 COMP-3 VALUE +0002406.
           05  SCB-GRP-0220.
               10  SCB-C-002407             PIC X(24) VALUE 'FIELD-002407-ALPHA'.
               10  SCB-D-002408             PIC 9(04) VALUE 6856.
               10  SCB-DESC-002408          PIC X(18) VALUE 'DESC-031304'.
               10  SCB-E-002409             OCCURS 3 TIMES.
                   15  SCB-EK-002409        PIC X(10).
                   15  SCB-EV-002409        PIC 9(06) COMP.
               10  SCB-A-002410             PIC 9(09) COMP VALUE 2410.
               10  SCB-B-002411             PIC S9(07)V99 COMP-3 VALUE +0002411.
               10  SCB-C-002412             PIC X(24) VALUE 'FIELD-002412-ALPHA'.
               10  SCB-D-002413             PIC 9(04) VALUE 6891.
               10  SCB-DESC-002413          PIC X(18) VALUE 'DESC-031369'.
               10  SCB-E-002414             OCCURS 4 TIMES.
                   15  SCB-EK-002414        PIC X(10).
                   15  SCB-EV-002414        PIC 9(06) COMP.
               10  SCB-A-002415             PIC 9(09) COMP VALUE 2415.
               10  SCB-B-002416             PIC S9(07)V99 COMP-3 VALUE +0002416.
               10  SCB-C-002417             PIC X(24) VALUE 'FIELD-002417-ALPHA'.
           05  SCB-GRP-0221.
               10  SCB-D-002418             PIC 9(04) VALUE 6926.
               10  SCB-DESC-002418          PIC X(18) VALUE 'DESC-031434'.
               10  SCB-E-002419             OCCURS 5 TIMES.
                   15  SCB-EK-002419        PIC X(10).
                   15  SCB-EV-002419        PIC 9(06) COMP.
               10  SCB-A-002420             PIC 9(09) COMP VALUE 2420.
               10  SCB-B-002421             PIC S9(07)V99 COMP-3 VALUE +0002421.
               10  SCB-C-002422             PIC X(24) VALUE 'FIELD-002422-ALPHA'.
               10  SCB-D-002423             PIC 9(04) VALUE 6961.
               10  SCB-DESC-002423          PIC X(18) VALUE 'DESC-031499'.
               10  SCB-E-002424             OCCURS 2 TIMES.
                   15  SCB-EK-002424        PIC X(10).
                   15  SCB-EV-002424        PIC 9(06) COMP.
               10  SCB-A-002425             PIC 9(09) COMP VALUE 2425.
               10  SCB-B-002426             PIC S9(07)V99 COMP-3 VALUE +0002426.
               10  SCB-C-002427             PIC X(24) VALUE 'FIELD-002427-ALPHA'.
               10  SCB-D-002428             PIC 9(04) VALUE 6996.
               10  SCB-DESC-002428          PIC X(18) VALUE 'DESC-031564'.
               10  SCB-E-002429             OCCURS 3 TIMES.
                   15  SCB-EK-002429        PIC X(10).
                   15  SCB-EV-002429        PIC 9(06) COMP.
           05  SCB-GRP-0222.
               10  SCB-A-002430             PIC 9(09) COMP VALUE 2430.
               10  SCB-B-002431             PIC S9(07)V99 COMP-3 VALUE +0002431.
               10  SCB-C-002432             PIC X(24) VALUE 'FIELD-002432-ALPHA'.
               10  SCB-D-002433             PIC 9(04) VALUE 7031.
               10  SCB-DESC-002433          PIC X(18) VALUE 'DESC-031629'.
               10  SCB-E-002434             OCCURS 4 TIMES.
                   15  SCB-EK-002434        PIC X(10).
                   15  SCB-EV-002434        PIC 9(06) COMP.
               10  SCB-A-002435             PIC 9(09) COMP VALUE 2435.
               10  SCB-B-002436             PIC S9(07)V99 COMP-3 VALUE +0002436.
               10  SCB-C-002437             PIC X(24) VALUE 'FIELD-002437-ALPHA'.
               10  SCB-D-002438             PIC 9(04) VALUE 7066.
               10  SCB-DESC-002438          PIC X(18) VALUE 'DESC-031694'.
               10  SCB-E-002439             OCCURS 5 TIMES.
                   15  SCB-EK-002439        PIC X(10).
                   15  SCB-EV-002439        PIC 9(06) COMP.
               10  SCB-A-002440             PIC 9(09) COMP VALUE 2440.
               10  SCB-B-002441             PIC S9(07)V99 COMP-3 VALUE +0002441.
               10  SCB-C-002442             PIC X(24) VALUE 'FIELD-002442-ALPHA'.
           05  SCB-ALT-0037 REDEFINES SCB-GRP-0222.
               10  SCB-ALT-FLAG-0037      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0037   PIC X(64).
           05  SCB-GRP-0223.
               10  SCB-D-002443             PIC 9(04) VALUE 7101.
               10  SCB-DESC-002443          PIC X(18) VALUE 'DESC-031759'.
               10  SCB-E-002444             OCCURS 2 TIMES.
                   15  SCB-EK-002444        PIC X(10).
                   15  SCB-EV-002444        PIC 9(06) COMP.
               10  SCB-A-002445             PIC 9(09) COMP VALUE 2445.
               10  SCB-B-002446             PIC S9(07)V99 COMP-3 VALUE +0002446.
               10  SCB-C-002447             PIC X(24) VALUE 'FIELD-002447-ALPHA'.
               10  SCB-D-002448             PIC 9(04) VALUE 7136.
               10  SCB-DESC-002448          PIC X(18) VALUE 'DESC-031824'.
               10  SCB-E-002449             OCCURS 3 TIMES.
                   15  SCB-EK-002449        PIC X(10).
                   15  SCB-EV-002449        PIC 9(06) COMP.
               10  SCB-A-002450             PIC 9(09) COMP VALUE 2450.
               10  SCB-B-002451             PIC S9(07)V99 COMP-3 VALUE +0002451.
               10  SCB-C-002452             PIC X(24) VALUE 'FIELD-002452-ALPHA'.
               10  SCB-D-002453             PIC 9(04) VALUE 7171.
               10  SCB-DESC-002453          PIC X(18) VALUE 'DESC-031889'.
               10  SCB-E-002454             OCCURS 4 TIMES.
                   15  SCB-EK-002454        PIC X(10).
                   15  SCB-EV-002454        PIC 9(06) COMP.
               10  SCB-A-002455             PIC 9(09) COMP VALUE 2455.
               10  SCB-B-002456             PIC S9(07)V99 COMP-3 VALUE +0002456.
           05  SCB-GRP-0224.
               10  SCB-C-002457             PIC X(24) VALUE 'FIELD-002457-ALPHA'.
               10  SCB-D-002458             PIC 9(04) VALUE 7206.
               10  SCB-DESC-002458          PIC X(18) VALUE 'DESC-031954'.
               10  SCB-E-002459             OCCURS 5 TIMES.
                   15  SCB-EK-002459        PIC X(10).
                   15  SCB-EV-002459        PIC 9(06) COMP.
               10  SCB-A-002460             PIC 9(09) COMP VALUE 2460.
               10  SCB-B-002461             PIC S9(07)V99 COMP-3 VALUE +0002461.
               10  SCB-C-002462             PIC X(24) VALUE 'FIELD-002462-ALPHA'.
               10  SCB-D-002463             PIC 9(04) VALUE 7241.
               10  SCB-DESC-002463          PIC X(18) VALUE 'DESC-032019'.
               10  SCB-E-002464             OCCURS 2 TIMES.
                   15  SCB-EK-002464        PIC X(10).
                   15  SCB-EV-002464        PIC 9(06) COMP.
           05  SCB-GRP-0225.
               10  SCB-A-002465             PIC 9(09) COMP VALUE 2465.
               10  SCB-B-002466             PIC S9(07)V99 COMP-3 VALUE +0002466.
               10  SCB-C-002467             PIC X(24) VALUE 'FIELD-002467-ALPHA'.
               10  SCB-D-002468             PIC 9(04) VALUE 7276.
               10  SCB-DESC-002468          PIC X(18) VALUE 'DESC-032084'.
               10  SCB-E-002469             OCCURS 3 TIMES.
                   15  SCB-EK-002469        PIC X(10).
                   15  SCB-EV-002469        PIC 9(06) COMP.
               10  SCB-A-002470             PIC 9(09) COMP VALUE 2470.
               10  SCB-B-002471             PIC S9(07)V99 COMP-3 VALUE +0002471.
               10  SCB-C-002472             PIC X(24) VALUE 'FIELD-002472-ALPHA'.
               10  SCB-D-002473             PIC 9(04) VALUE 7311.
               10  SCB-DESC-002473          PIC X(18) VALUE 'DESC-032149'.
           05  SCB-GRP-0226.
               10  SCB-E-002474             OCCURS 4 TIMES.
                   15  SCB-EK-002474        PIC X(10).
                   15  SCB-EV-002474        PIC 9(06) COMP.
               10  SCB-A-002475             PIC 9(09) COMP VALUE 2475.
               10  SCB-B-002476             PIC S9(07)V99 COMP-3 VALUE +0002476.
               10  SCB-C-002477             PIC X(24) VALUE 'FIELD-002477-ALPHA'.
               10  SCB-D-002478             PIC 9(04) VALUE 7346.
               10  SCB-DESC-002478          PIC X(18) VALUE 'DESC-032214'.
               10  SCB-E-002479             OCCURS 5 TIMES.
                   15  SCB-EK-002479        PIC X(10).
                   15  SCB-EV-002479        PIC 9(06) COMP.
               10  SCB-A-002480             PIC 9(09) COMP VALUE 2480.
               10  SCB-B-002481             PIC S9(07)V99 COMP-3 VALUE +0002481.
               10  SCB-C-002482             PIC X(24) VALUE 'FIELD-002482-ALPHA'.
               10  SCB-D-002483             PIC 9(04) VALUE 7381.
               10  SCB-DESC-002483          PIC X(18) VALUE 'DESC-032279'.
           05  SCB-GRP-0227.
               10  SCB-E-002484             OCCURS 2 TIMES.
                   15  SCB-EK-002484        PIC X(10).
                   15  SCB-EV-002484        PIC 9(06) COMP.
               10  SCB-A-002485             PIC 9(09) COMP VALUE 2485.
               10  SCB-B-002486             PIC S9(07)V99 COMP-3 VALUE +0002486.
               10  SCB-C-002487             PIC X(24) VALUE 'FIELD-002487-ALPHA'.
               10  SCB-D-002488             PIC 9(04) VALUE 7416.
               10  SCB-DESC-002488          PIC X(18) VALUE 'DESC-032344'.
               10  SCB-E-002489             OCCURS 3 TIMES.
                   15  SCB-EK-002489        PIC X(10).
                   15  SCB-EV-002489        PIC 9(06) COMP.
               10  SCB-A-002490             PIC 9(09) COMP VALUE 2490.
               10  SCB-B-002491             PIC S9(07)V99 COMP-3 VALUE +0002491.
               10  SCB-C-002492             PIC X(24) VALUE 'FIELD-002492-ALPHA'.
               10  SCB-D-002493             PIC 9(04) VALUE 7451.
               10  SCB-DESC-002493          PIC X(18) VALUE 'DESC-032409'.
               10  SCB-E-002494             OCCURS 4 TIMES.
                   15  SCB-EK-002494        PIC X(10).
                   15  SCB-EV-002494        PIC 9(06) COMP.
           05  SCB-GRP-0228.
               10  SCB-A-002495             PIC 9(09) COMP VALUE 2495.
               10  SCB-B-002496             PIC S9(07)V99 COMP-3 VALUE +0002496.
               10  SCB-C-002497             PIC X(24) VALUE 'FIELD-002497-ALPHA'.
               10  SCB-D-002498             PIC 9(04) VALUE 7486.
               10  SCB-DESC-002498          PIC X(18) VALUE 'DESC-032474'.
               10  SCB-E-002499             OCCURS 5 TIMES.
                   15  SCB-EK-002499        PIC X(10).
                   15  SCB-EV-002499        PIC 9(06) COMP.
               10  SCB-A-002500             PIC 9(09) COMP VALUE 2500.
               10  SCB-B-002501             PIC S9(07)V99 COMP-3 VALUE +0002501.
               10  SCB-C-002502             PIC X(24) VALUE 'FIELD-002502-ALPHA'.
               10  SCB-D-002503             PIC 9(04) VALUE 7521.
               10  SCB-DESC-002503          PIC X(18) VALUE 'DESC-032539'.
               10  SCB-E-002504             OCCURS 2 TIMES.
                   15  SCB-EK-002504        PIC X(10).
                   15  SCB-EV-002504        PIC 9(06) COMP.
               10  SCB-A-002505             PIC 9(09) COMP VALUE 2505.
               10  SCB-B-002506             PIC S9(07)V99 COMP-3 VALUE +0002506.
           05  SCB-ALT-0038 REDEFINES SCB-GRP-0228.
               10  SCB-ALT-FLAG-0038      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0038   PIC X(64).
           05  SCB-GRP-0229.
               10  SCB-C-002507             PIC X(24) VALUE 'FIELD-002507-ALPHA'.
               10  SCB-D-002508             PIC 9(04) VALUE 7556.
               10  SCB-DESC-002508          PIC X(18) VALUE 'DESC-032604'.
               10  SCB-E-002509             OCCURS 3 TIMES.
                   15  SCB-EK-002509        PIC X(10).
                   15  SCB-EV-002509        PIC 9(06) COMP.
               10  SCB-A-002510             PIC 9(09) COMP VALUE 2510.
               10  SCB-B-002511             PIC S9(07)V99 COMP-3 VALUE +0002511.
               10  SCB-C-002512             PIC X(24) VALUE 'FIELD-002512-ALPHA'.
               10  SCB-D-002513             PIC 9(04) VALUE 7591.
               10  SCB-DESC-002513          PIC X(18) VALUE 'DESC-032669'.
               10  SCB-E-002514             OCCURS 4 TIMES.
                   15  SCB-EK-002514        PIC X(10).
                   15  SCB-EV-002514        PIC 9(06) COMP.
               10  SCB-A-002515             PIC 9(09) COMP VALUE 2515.
               10  SCB-B-002516             PIC S9(07)V99 COMP-3 VALUE +0002516.
               10  SCB-C-002517             PIC X(24) VALUE 'FIELD-002517-ALPHA'.
               10  SCB-D-002518             PIC 9(04) VALUE 7626.
               10  SCB-DESC-002518          PIC X(18) VALUE 'DESC-032734'.
               10  SCB-E-002519             OCCURS 5 TIMES.
                   15  SCB-EK-002519        PIC X(10).
                   15  SCB-EV-002519        PIC 9(06) COMP.
           05  SCB-GRP-0230.
               10  SCB-A-002520             PIC 9(09) COMP VALUE 2520.
               10  SCB-B-002521             PIC S9(07)V99 COMP-3 VALUE +0002521.
               10  SCB-C-002522             PIC X(24) VALUE 'FIELD-002522-ALPHA'.
               10  SCB-D-002523             PIC 9(04) VALUE 7661.
               10  SCB-DESC-002523          PIC X(18) VALUE 'DESC-032799'.
               10  SCB-E-002524             OCCURS 2 TIMES.
                   15  SCB-EK-002524        PIC X(10).
                   15  SCB-EV-002524        PIC 9(06) COMP.
               10  SCB-A-002525             PIC 9(09) COMP VALUE 2525.
               10  SCB-B-002526             PIC S9(07)V99 COMP-3 VALUE +0002526.
               10  SCB-C-002527             PIC X(24) VALUE 'FIELD-002527-ALPHA'.
               10  SCB-D-002528             PIC 9(04) VALUE 7696.
               10  SCB-DESC-002528          PIC X(18) VALUE 'DESC-032864'.
               10  SCB-E-002529             OCCURS 3 TIMES.
                   15  SCB-EK-002529        PIC X(10).
                   15  SCB-EV-002529        PIC 9(06) COMP.
               10  SCB-A-002530             PIC 9(09) COMP VALUE 2530.
               10  SCB-B-002531             PIC S9(07)V99 COMP-3 VALUE +0002531.
               10  SCB-C-002532             PIC X(24) VALUE 'FIELD-002532-ALPHA'.
               10  SCB-D-002533             PIC 9(04) VALUE 7731.
               10  SCB-DESC-002533          PIC X(18) VALUE 'DESC-032929'.
           05  SCB-GRP-0231.
               10  SCB-E-002534             OCCURS 4 TIMES.
                   15  SCB-EK-002534        PIC X(10).
                   15  SCB-EV-002534        PIC 9(06) COMP.
               10  SCB-A-002535             PIC 9(09) COMP VALUE 2535.
               10  SCB-B-002536             PIC S9(07)V99 COMP-3 VALUE +0002536.
               10  SCB-C-002537             PIC X(24) VALUE 'FIELD-002537-ALPHA'.
               10  SCB-D-002538             PIC 9(04) VALUE 7766.
               10  SCB-DESC-002538          PIC X(18) VALUE 'DESC-032994'.
               10  SCB-E-002539             OCCURS 5 TIMES.
                   15  SCB-EK-002539        PIC X(10).
                   15  SCB-EV-002539        PIC 9(06) COMP.
               10  SCB-A-002540             PIC 9(09) COMP VALUE 2540.
               10  SCB-B-002541             PIC S9(07)V99 COMP-3 VALUE +0002541.
           05  SCB-GRP-0232.
               10  SCB-C-002542             PIC X(24) VALUE 'FIELD-002542-ALPHA'.
               10  SCB-D-002543             PIC 9(04) VALUE 7801.
               10  SCB-DESC-002543          PIC X(18) VALUE 'DESC-033059'.
               10  SCB-E-002544             OCCURS 2 TIMES.
                   15  SCB-EK-002544        PIC X(10).
                   15  SCB-EV-002544        PIC 9(06) COMP.
               10  SCB-A-002545             PIC 9(09) COMP VALUE 2545.
               10  SCB-B-002546             PIC S9(07)V99 COMP-3 VALUE +0002546.
               10  SCB-C-002547             PIC X(24) VALUE 'FIELD-002547-ALPHA'.
               10  SCB-D-002548             PIC 9(04) VALUE 7836.
               10  SCB-DESC-002548          PIC X(18) VALUE 'DESC-033124'.
               10  SCB-E-002549             OCCURS 3 TIMES.
                   15  SCB-EK-002549        PIC X(10).
                   15  SCB-EV-002549        PIC 9(06) COMP.
               10  SCB-A-002550             PIC 9(09) COMP VALUE 2550.
           05  SCB-GRP-0233.
               10  SCB-B-002551             PIC S9(07)V99 COMP-3 VALUE +0002551.
               10  SCB-C-002552             PIC X(24) VALUE 'FIELD-002552-ALPHA'.
               10  SCB-D-002553             PIC 9(04) VALUE 7871.
               10  SCB-DESC-002553          PIC X(18) VALUE 'DESC-033189'.
               10  SCB-E-002554             OCCURS 4 TIMES.
                   15  SCB-EK-002554        PIC X(10).
                   15  SCB-EV-002554        PIC 9(06) COMP.
               10  SCB-A-002555             PIC 9(09) COMP VALUE 2555.
               10  SCB-B-002556             PIC S9(07)V99 COMP-3 VALUE +0002556.
               10  SCB-C-002557             PIC X(24) VALUE 'FIELD-002557-ALPHA'.
               10  SCB-D-002558             PIC 9(04) VALUE 7906.
               10  SCB-DESC-002558          PIC X(18) VALUE 'DESC-033254'.
               10  SCB-E-002559             OCCURS 5 TIMES.
                   15  SCB-EK-002559        PIC X(10).
                   15  SCB-EV-002559        PIC 9(06) COMP.
               10  SCB-A-002560             PIC 9(09) COMP VALUE 2560.
           05  SCB-GRP-0234.
               10  SCB-B-002561             PIC S9(07)V99 COMP-3 VALUE +0002561.
               10  SCB-C-002562             PIC X(24) VALUE 'FIELD-002562-ALPHA'.
               10  SCB-D-002563             PIC 9(04) VALUE 7941.
               10  SCB-DESC-002563          PIC X(18) VALUE 'DESC-033319'.
               10  SCB-E-002564             OCCURS 2 TIMES.
                   15  SCB-EK-002564        PIC X(10).
                   15  SCB-EV-002564        PIC 9(06) COMP.
               10  SCB-A-002565             PIC 9(09) COMP VALUE 2565.
               10  SCB-B-002566             PIC S9(07)V99 COMP-3 VALUE +0002566.
               10  SCB-C-002567             PIC X(24) VALUE 'FIELD-002567-ALPHA'.
               10  SCB-D-002568             PIC 9(04) VALUE 7976.
               10  SCB-DESC-002568          PIC X(18) VALUE 'DESC-033384'.
               10  SCB-E-002569             OCCURS 3 TIMES.
                   15  SCB-EK-002569        PIC X(10).
                   15  SCB-EV-002569        PIC 9(06) COMP.
               10  SCB-A-002570             PIC 9(09) COMP VALUE 2570.
               10  SCB-B-002571             PIC S9(07)V99 COMP-3 VALUE +0002571.
           05  SCB-ALT-0039 REDEFINES SCB-GRP-0234.
               10  SCB-ALT-FLAG-0039      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0039   PIC X(64).
           05  SCB-GRP-0235.
               10  SCB-C-002572             PIC X(24) VALUE 'FIELD-002572-ALPHA'.
               10  SCB-D-002573             PIC 9(04) VALUE 8011.
               10  SCB-DESC-002573          PIC X(18) VALUE 'DESC-033449'.
               10  SCB-E-002574             OCCURS 4 TIMES.
                   15  SCB-EK-002574        PIC X(10).
                   15  SCB-EV-002574        PIC 9(06) COMP.
               10  SCB-A-002575             PIC 9(09) COMP VALUE 2575.
               10  SCB-B-002576             PIC S9(07)V99 COMP-3 VALUE +0002576.
               10  SCB-C-002577             PIC X(24) VALUE 'FIELD-002577-ALPHA'.
               10  SCB-D-002578             PIC 9(04) VALUE 8046.
               10  SCB-DESC-002578          PIC X(18) VALUE 'DESC-033514'.
               10  SCB-E-002579             OCCURS 5 TIMES.
                   15  SCB-EK-002579        PIC X(10).
                   15  SCB-EV-002579        PIC 9(06) COMP.
               10  SCB-A-002580             PIC 9(09) COMP VALUE 2580.
               10  SCB-B-002581             PIC S9(07)V99 COMP-3 VALUE +0002581.
               10  SCB-C-002582             PIC X(24) VALUE 'FIELD-002582-ALPHA'.
               10  SCB-D-002583             PIC 9(04) VALUE 8081.
               10  SCB-DESC-002583          PIC X(18) VALUE 'DESC-033579'.
           05  SCB-GRP-0236.
               10  SCB-E-002584             OCCURS 2 TIMES.
                   15  SCB-EK-002584        PIC X(10).
                   15  SCB-EV-002584        PIC 9(06) COMP.
               10  SCB-A-002585             PIC 9(09) COMP VALUE 2585.
               10  SCB-B-002586             PIC S9(07)V99 COMP-3 VALUE +0002586.
               10  SCB-C-002587             PIC X(24) VALUE 'FIELD-002587-ALPHA'.
               10  SCB-D-002588             PIC 9(04) VALUE 8116.
               10  SCB-DESC-002588          PIC X(18) VALUE 'DESC-033644'.
               10  SCB-E-002589             OCCURS 3 TIMES.
                   15  SCB-EK-002589        PIC X(10).
                   15  SCB-EV-002589        PIC 9(06) COMP.
               10  SCB-A-002590             PIC 9(09) COMP VALUE 2590.
               10  SCB-B-002591             PIC S9(07)V99 COMP-3 VALUE +0002591.
               10  SCB-C-002592             PIC X(24) VALUE 'FIELD-002592-ALPHA'.
               10  SCB-D-002593             PIC 9(04) VALUE 8151.
               10  SCB-DESC-002593          PIC X(18) VALUE 'DESC-033709'.
               10  SCB-E-002594             OCCURS 4 TIMES.
                   15  SCB-EK-002594        PIC X(10).
                   15  SCB-EV-002594        PIC 9(06) COMP.
               10  SCB-A-002595             PIC 9(09) COMP VALUE 2595.
               10  SCB-B-002596             PIC S9(07)V99 COMP-3 VALUE +0002596.
           05  SCB-GRP-0237.
               10  SCB-C-002597             PIC X(24) VALUE 'FIELD-002597-ALPHA'.
               10  SCB-D-002598             PIC 9(04) VALUE 8186.
               10  SCB-DESC-002598          PIC X(18) VALUE 'DESC-033774'.
               10  SCB-E-002599             OCCURS 5 TIMES.
                   15  SCB-EK-002599        PIC X(10).
                   15  SCB-EV-002599        PIC 9(06) COMP.
               10  SCB-A-002600             PIC 9(09) COMP VALUE 2600.
               10  SCB-B-002601             PIC S9(07)V99 COMP-3 VALUE +0002601.
               10  SCB-C-002602             PIC X(24) VALUE 'FIELD-002602-ALPHA'.
               10  SCB-D-002603             PIC 9(04) VALUE 8221.
               10  SCB-DESC-002603          PIC X(18) VALUE 'DESC-033839'.
               10  SCB-E-002604             OCCURS 2 TIMES.
                   15  SCB-EK-002604        PIC X(10).
                   15  SCB-EV-002604        PIC 9(06) COMP.
               10  SCB-A-002605             PIC 9(09) COMP VALUE 2605.
               10  SCB-B-002606             PIC S9(07)V99 COMP-3 VALUE +0002606.
               10  SCB-C-002607             PIC X(24) VALUE 'FIELD-002607-ALPHA'.
               10  SCB-D-002608             PIC 9(04) VALUE 8256.
               10  SCB-DESC-002608          PIC X(18) VALUE 'DESC-033904'.
               10  SCB-E-002609             OCCURS 3 TIMES.
                   15  SCB-EK-002609        PIC X(10).
                   15  SCB-EV-002609        PIC 9(06) COMP.
               10  SCB-A-002610             PIC 9(09) COMP VALUE 2610.
           05  SCB-GRP-0238.
               10  SCB-B-002611             PIC S9(07)V99 COMP-3 VALUE +0002611.
               10  SCB-C-002612             PIC X(24) VALUE 'FIELD-002612-ALPHA'.
               10  SCB-D-002613             PIC 9(04) VALUE 8291.
               10  SCB-DESC-002613          PIC X(18) VALUE 'DESC-033969'.
               10  SCB-E-002614             OCCURS 4 TIMES.
                   15  SCB-EK-002614        PIC X(10).
                   15  SCB-EV-002614        PIC 9(06) COMP.
               10  SCB-A-002615             PIC 9(09) COMP VALUE 2615.
               10  SCB-B-002616             PIC S9(07)V99 COMP-3 VALUE +0002616.
               10  SCB-C-002617             PIC X(24) VALUE 'FIELD-002617-ALPHA'.
               10  SCB-D-002618             PIC 9(04) VALUE 8326.
               10  SCB-DESC-002618          PIC X(18) VALUE 'DESC-034034'.
           05  SCB-GRP-0239.
               10  SCB-E-002619             OCCURS 5 TIMES.
                   15  SCB-EK-002619        PIC X(10).
                   15  SCB-EV-002619        PIC 9(06) COMP.
               10  SCB-A-002620             PIC 9(09) COMP VALUE 2620.
               10  SCB-B-002621             PIC S9(07)V99 COMP-3 VALUE +0002621.
               10  SCB-C-002622             PIC X(24) VALUE 'FIELD-002622-ALPHA'.
               10  SCB-D-002623             PIC 9(04) VALUE 8361.
               10  SCB-DESC-002623          PIC X(18) VALUE 'DESC-034099'.
               10  SCB-E-002624             OCCURS 2 TIMES.
                   15  SCB-EK-002624        PIC X(10).
                   15  SCB-EV-002624        PIC 9(06) COMP.
               10  SCB-A-002625             PIC 9(09) COMP VALUE 2625.
               10  SCB-B-002626             PIC S9(07)V99 COMP-3 VALUE +0002626.
               10  SCB-C-002627             PIC X(24) VALUE 'FIELD-002627-ALPHA'.
           05  SCB-GRP-0240.
               10  SCB-D-002628             PIC 9(04) VALUE 8396.
               10  SCB-DESC-002628          PIC X(18) VALUE 'DESC-034164'.
               10  SCB-E-002629             OCCURS 3 TIMES.
                   15  SCB-EK-002629        PIC X(10).
                   15  SCB-EV-002629        PIC 9(06) COMP.
               10  SCB-A-002630             PIC 9(09) COMP VALUE 2630.
               10  SCB-B-002631             PIC S9(07)V99 COMP-3 VALUE +0002631.
               10  SCB-C-002632             PIC X(24) VALUE 'FIELD-002632-ALPHA'.
               10  SCB-D-002633             PIC 9(04) VALUE 8431.
               10  SCB-DESC-002633          PIC X(18) VALUE 'DESC-034229'.
               10  SCB-E-002634             OCCURS 4 TIMES.
                   15  SCB-EK-002634        PIC X(10).
                   15  SCB-EV-002634        PIC 9(06) COMP.
               10  SCB-A-002635             PIC 9(09) COMP VALUE 2635.
               10  SCB-B-002636             PIC S9(07)V99 COMP-3 VALUE +0002636.
               10  SCB-C-002637             PIC X(24) VALUE 'FIELD-002637-ALPHA'.
           05  SCB-ALT-0040 REDEFINES SCB-GRP-0240.
               10  SCB-ALT-FLAG-0040      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0040   PIC X(64).
           05  SCB-GRP-0241.
               10  SCB-D-002638             PIC 9(04) VALUE 8466.
               10  SCB-DESC-002638          PIC X(18) VALUE 'DESC-034294'.
               10  SCB-E-002639             OCCURS 5 TIMES.
                   15  SCB-EK-002639        PIC X(10).
                   15  SCB-EV-002639        PIC 9(06) COMP.
               10  SCB-A-002640             PIC 9(09) COMP VALUE 2640.
               10  SCB-B-002641             PIC S9(07)V99 COMP-3 VALUE +0002641.
               10  SCB-C-002642             PIC X(24) VALUE 'FIELD-002642-ALPHA'.
               10  SCB-D-002643             PIC 9(04) VALUE 8501.
               10  SCB-DESC-002643          PIC X(18) VALUE 'DESC-034359'.
               10  SCB-E-002644             OCCURS 2 TIMES.
                   15  SCB-EK-002644        PIC X(10).
                   15  SCB-EV-002644        PIC 9(06) COMP.
               10  SCB-A-002645             PIC 9(09) COMP VALUE 2645.
               10  SCB-B-002646             PIC S9(07)V99 COMP-3 VALUE +0002646.
               10  SCB-C-002647             PIC X(24) VALUE 'FIELD-002647-ALPHA'.
               10  SCB-D-002648             PIC 9(04) VALUE 8536.
               10  SCB-DESC-002648          PIC X(18) VALUE 'DESC-034424'.
           05  SCB-GRP-0242.
               10  SCB-E-002649             OCCURS 3 TIMES.
                   15  SCB-EK-002649        PIC X(10).
                   15  SCB-EV-002649        PIC 9(06) COMP.
               10  SCB-A-002650             PIC 9(09) COMP VALUE 2650.
               10  SCB-B-002651             PIC S9(07)V99 COMP-3 VALUE +0002651.
               10  SCB-C-002652             PIC X(24) VALUE 'FIELD-002652-ALPHA'.
               10  SCB-D-002653             PIC 9(04) VALUE 8571.
               10  SCB-DESC-002653          PIC X(18) VALUE 'DESC-034489'.
               10  SCB-E-002654             OCCURS 4 TIMES.
                   15  SCB-EK-002654        PIC X(10).
                   15  SCB-EV-002654        PIC 9(06) COMP.
               10  SCB-A-002655             PIC 9(09) COMP VALUE 2655.
               10  SCB-B-002656             PIC S9(07)V99 COMP-3 VALUE +0002656.
               10  SCB-C-002657             PIC X(24) VALUE 'FIELD-002657-ALPHA'.
               10  SCB-D-002658             PIC 9(04) VALUE 8606.
               10  SCB-DESC-002658          PIC X(18) VALUE 'DESC-034554'.
               10  SCB-E-002659             OCCURS 5 TIMES.
                   15  SCB-EK-002659        PIC X(10).
                   15  SCB-EV-002659        PIC 9(06) COMP.
               10  SCB-A-002660             PIC 9(09) COMP VALUE 2660.
           05  SCB-GRP-0243.
               10  SCB-B-002661             PIC S9(07)V99 COMP-3 VALUE +0002661.
               10  SCB-C-002662             PIC X(24) VALUE 'FIELD-002662-ALPHA'.
               10  SCB-D-002663             PIC 9(04) VALUE 8641.
               10  SCB-DESC-002663          PIC X(18) VALUE 'DESC-034619'.
               10  SCB-E-002664             OCCURS 2 TIMES.
                   15  SCB-EK-002664        PIC X(10).
                   15  SCB-EV-002664        PIC 9(06) COMP.
               10  SCB-A-002665             PIC 9(09) COMP VALUE 2665.
               10  SCB-B-002666             PIC S9(07)V99 COMP-3 VALUE +0002666.
               10  SCB-C-002667             PIC X(24) VALUE 'FIELD-002667-ALPHA'.
               10  SCB-D-002668             PIC 9(04) VALUE 8676.
               10  SCB-DESC-002668          PIC X(18) VALUE 'DESC-034684'.
               10  SCB-E-002669             OCCURS 3 TIMES.
                   15  SCB-EK-002669        PIC X(10).
                   15  SCB-EV-002669        PIC 9(06) COMP.
               10  SCB-A-002670             PIC 9(09) COMP VALUE 2670.
               10  SCB-B-002671             PIC S9(07)V99 COMP-3 VALUE +0002671.
               10  SCB-C-002672             PIC X(24) VALUE 'FIELD-002672-ALPHA'.
               10  SCB-D-002673             PIC 9(04) VALUE 8711.
               10  SCB-DESC-002673          PIC X(18) VALUE 'DESC-034749'.
           05  SCB-GRP-0244.
               10  SCB-E-002674             OCCURS 4 TIMES.
                   15  SCB-EK-002674        PIC X(10).
                   15  SCB-EV-002674        PIC 9(06) COMP.
               10  SCB-A-002675             PIC 9(09) COMP VALUE 2675.
               10  SCB-B-002676             PIC S9(07)V99 COMP-3 VALUE +0002676.
               10  SCB-C-002677             PIC X(24) VALUE 'FIELD-002677-ALPHA'.
               10  SCB-D-002678             PIC 9(04) VALUE 8746.
               10  SCB-DESC-002678          PIC X(18) VALUE 'DESC-034814'.
               10  SCB-E-002679             OCCURS 5 TIMES.
                   15  SCB-EK-002679        PIC X(10).
                   15  SCB-EV-002679        PIC 9(06) COMP.
               10  SCB-A-002680             PIC 9(09) COMP VALUE 2680.
               10  SCB-B-002681             PIC S9(07)V99 COMP-3 VALUE +0002681.
               10  SCB-C-002682             PIC X(24) VALUE 'FIELD-002682-ALPHA'.
               10  SCB-D-002683             PIC 9(04) VALUE 8781.
               10  SCB-DESC-002683          PIC X(18) VALUE 'DESC-034879'.
               10  SCB-E-002684             OCCURS 2 TIMES.
                   15  SCB-EK-002684        PIC X(10).
                   15  SCB-EV-002684        PIC 9(06) COMP.
               10  SCB-A-002685             PIC 9(09) COMP VALUE 2685.
               10  SCB-B-002686             PIC S9(07)V99 COMP-3 VALUE +0002686.
               10  SCB-C-002687             PIC X(24) VALUE 'FIELD-002687-ALPHA'.
           05  SCB-GRP-0245.
               10  SCB-D-002688             PIC 9(04) VALUE 8816.
               10  SCB-DESC-002688          PIC X(18) VALUE 'DESC-034944'.
               10  SCB-E-002689             OCCURS 3 TIMES.
                   15  SCB-EK-002689        PIC X(10).
                   15  SCB-EV-002689        PIC 9(06) COMP.
               10  SCB-A-002690             PIC 9(09) COMP VALUE 2690.
               10  SCB-B-002691             PIC S9(07)V99 COMP-3 VALUE +0002691.
               10  SCB-C-002692             PIC X(24) VALUE 'FIELD-002692-ALPHA'.
               10  SCB-D-002693             PIC 9(04) VALUE 8851.
               10  SCB-DESC-002693          PIC X(18) VALUE 'DESC-035009'.
               10  SCB-E-002694             OCCURS 4 TIMES.
                   15  SCB-EK-002694        PIC X(10).
                   15  SCB-EV-002694        PIC 9(06) COMP.
               10  SCB-A-002695             PIC 9(09) COMP VALUE 2695.
           05  SCB-GRP-0246.
               10  SCB-B-002696             PIC S9(07)V99 COMP-3 VALUE +0002696.
               10  SCB-C-002697             PIC X(24) VALUE 'FIELD-002697-ALPHA'.
               10  SCB-D-002698             PIC 9(04) VALUE 8886.
               10  SCB-DESC-002698          PIC X(18) VALUE 'DESC-035074'.
               10  SCB-E-002699             OCCURS 5 TIMES.
                   15  SCB-EK-002699        PIC X(10).
                   15  SCB-EV-002699        PIC 9(06) COMP.
               10  SCB-A-002700             PIC 9(09) COMP VALUE 2700.
               10  SCB-B-002701             PIC S9(07)V99 COMP-3 VALUE +0002701.
               10  SCB-C-002702             PIC X(24) VALUE 'FIELD-002702-ALPHA'.
               10  SCB-D-002703             PIC 9(04) VALUE 8921.
               10  SCB-DESC-002703          PIC X(18) VALUE 'DESC-035139'.
               10  SCB-E-002704             OCCURS 2 TIMES.
                   15  SCB-EK-002704        PIC X(10).
                   15  SCB-EV-002704        PIC 9(06) COMP.
           05  SCB-ALT-0041 REDEFINES SCB-GRP-0246.
               10  SCB-ALT-FLAG-0041      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0041   PIC X(64).
           05  SCB-GRP-0247.
               10  SCB-A-002705             PIC 9(09) COMP VALUE 2705.
               10  SCB-B-002706             PIC S9(07)V99 COMP-3 VALUE +0002706.
               10  SCB-C-002707             PIC X(24) VALUE 'FIELD-002707-ALPHA'.
               10  SCB-D-002708             PIC 9(04) VALUE 8956.
               10  SCB-DESC-002708          PIC X(18) VALUE 'DESC-035204'.
               10  SCB-E-002709             OCCURS 3 TIMES.
                   15  SCB-EK-002709        PIC X(10).
                   15  SCB-EV-002709        PIC 9(06) COMP.
               10  SCB-A-002710             PIC 9(09) COMP VALUE 2710.
               10  SCB-B-002711             PIC S9(07)V99 COMP-3 VALUE +0002711.
               10  SCB-C-002712             PIC X(24) VALUE 'FIELD-002712-ALPHA'.
               10  SCB-D-002713             PIC 9(04) VALUE 8991.
               10  SCB-DESC-002713          PIC X(18) VALUE 'DESC-035269'.
               10  SCB-E-002714             OCCURS 4 TIMES.
                   15  SCB-EK-002714        PIC X(10).
                   15  SCB-EV-002714        PIC 9(06) COMP.
           05  SCB-GRP-0248.
               10  SCB-A-002715             PIC 9(09) COMP VALUE 2715.
               10  SCB-B-002716             PIC S9(07)V99 COMP-3 VALUE +0002716.
               10  SCB-C-002717             PIC X(24) VALUE 'FIELD-002717-ALPHA'.
               10  SCB-D-002718             PIC 9(04) VALUE 9026.
               10  SCB-DESC-002718          PIC X(18) VALUE 'DESC-035334'.
               10  SCB-E-002719             OCCURS 5 TIMES.
                   15  SCB-EK-002719        PIC X(10).
                   15  SCB-EV-002719        PIC 9(06) COMP.
               10  SCB-A-002720             PIC 9(09) COMP VALUE 2720.
               10  SCB-B-002721             PIC S9(07)V99 COMP-3 VALUE +0002721.
               10  SCB-C-002722             PIC X(24) VALUE 'FIELD-002722-ALPHA'.
               10  SCB-D-002723             PIC 9(04) VALUE 9061.
               10  SCB-DESC-002723          PIC X(18) VALUE 'DESC-035399'.
               10  SCB-E-002724             OCCURS 2 TIMES.
                   15  SCB-EK-002724        PIC X(10).
                   15  SCB-EV-002724        PIC 9(06) COMP.
               10  SCB-A-002725             PIC 9(09) COMP VALUE 2725.
           05  SCB-GRP-0249.
               10  SCB-B-002726             PIC S9(07)V99 COMP-3 VALUE +0002726.
               10  SCB-C-002727             PIC X(24) VALUE 'FIELD-002727-ALPHA'.
               10  SCB-D-002728             PIC 9(04) VALUE 9096.
               10  SCB-DESC-002728          PIC X(18) VALUE 'DESC-035464'.
               10  SCB-E-002729             OCCURS 3 TIMES.
                   15  SCB-EK-002729        PIC X(10).
                   15  SCB-EV-002729        PIC 9(06) COMP.
               10  SCB-A-002730             PIC 9(09) COMP VALUE 2730.
               10  SCB-B-002731             PIC S9(07)V99 COMP-3 VALUE +0002731.
               10  SCB-C-002732             PIC X(24) VALUE 'FIELD-002732-ALPHA'.
               10  SCB-D-002733             PIC 9(04) VALUE 9131.
               10  SCB-DESC-002733          PIC X(18) VALUE 'DESC-035529'.
               10  SCB-E-002734             OCCURS 4 TIMES.
                   15  SCB-EK-002734        PIC X(10).
                   15  SCB-EV-002734        PIC 9(06) COMP.
               10  SCB-A-002735             PIC 9(09) COMP VALUE 2735.
               10  SCB-B-002736             PIC S9(07)V99 COMP-3 VALUE +0002736.
               10  SCB-C-002737             PIC X(24) VALUE 'FIELD-002737-ALPHA'.
           05  SCB-GRP-0250.
               10  SCB-D-002738             PIC 9(04) VALUE 9166.
               10  SCB-DESC-002738          PIC X(18) VALUE 'DESC-035594'.
               10  SCB-E-002739             OCCURS 5 TIMES.
                   15  SCB-EK-002739        PIC X(10).
                   15  SCB-EV-002739        PIC 9(06) COMP.
               10  SCB-A-002740             PIC 9(09) COMP VALUE 2740.
               10  SCB-B-002741             PIC S9(07)V99 COMP-3 VALUE +0002741.
               10  SCB-C-002742             PIC X(24) VALUE 'FIELD-002742-ALPHA'.
               10  SCB-D-002743             PIC 9(04) VALUE 9201.
               10  SCB-DESC-002743          PIC X(18) VALUE 'DESC-035659'.
               10  SCB-E-002744             OCCURS 2 TIMES.
                   15  SCB-EK-002744        PIC X(10).
                   15  SCB-EV-002744        PIC 9(06) COMP.
               10  SCB-A-002745             PIC 9(09) COMP VALUE 2745.
               10  SCB-B-002746             PIC S9(07)V99 COMP-3 VALUE +0002746.
               10  SCB-C-002747             PIC X(24) VALUE 'FIELD-002747-ALPHA'.
               10  SCB-D-002748             PIC 9(04) VALUE 9236.
               10  SCB-DESC-002748          PIC X(18) VALUE 'DESC-035724'.
               10  SCB-E-002749             OCCURS 3 TIMES.
                   15  SCB-EK-002749        PIC X(10).
                   15  SCB-EV-002749        PIC 9(06) COMP.
               10  SCB-A-002750             PIC 9(09) COMP VALUE 2750.
           05  SCB-GRP-0251.
               10  SCB-B-002751             PIC S9(07)V99 COMP-3 VALUE +0002751.
               10  SCB-C-002752             PIC X(24) VALUE 'FIELD-002752-ALPHA'.
               10  SCB-D-002753             PIC 9(04) VALUE 9271.
               10  SCB-DESC-002753          PIC X(18) VALUE 'DESC-035789'.
               10  SCB-E-002754             OCCURS 4 TIMES.
                   15  SCB-EK-002754        PIC X(10).
                   15  SCB-EV-002754        PIC 9(06) COMP.
               10  SCB-A-002755             PIC 9(09) COMP VALUE 2755.
               10  SCB-B-002756             PIC S9(07)V99 COMP-3 VALUE +0002756.
               10  SCB-C-002757             PIC X(24) VALUE 'FIELD-002757-ALPHA'.
               10  SCB-D-002758             PIC 9(04) VALUE 9306.
               10  SCB-DESC-002758          PIC X(18) VALUE 'DESC-035854'.
               10  SCB-E-002759             OCCURS 5 TIMES.
                   15  SCB-EK-002759        PIC X(10).
                   15  SCB-EV-002759        PIC 9(06) COMP.
               10  SCB-A-002760             PIC 9(09) COMP VALUE 2760.
               10  SCB-B-002761             PIC S9(07)V99 COMP-3 VALUE +0002761.
               10  SCB-C-002762             PIC X(24) VALUE 'FIELD-002762-ALPHA'.
               10  SCB-D-002763             PIC 9(04) VALUE 9341.
               10  SCB-DESC-002763          PIC X(18) VALUE 'DESC-035919'.
               10  SCB-E-002764             OCCURS 2 TIMES.
                   15  SCB-EK-002764        PIC X(10).
                   15  SCB-EV-002764        PIC 9(06) COMP.
           05  SCB-GRP-0252.
               10  SCB-A-002765             PIC 9(09) COMP VALUE 2765.
               10  SCB-B-002766             PIC S9(07)V99 COMP-3 VALUE +0002766.
               10  SCB-C-002767             PIC X(24) VALUE 'FIELD-002767-ALPHA'.
               10  SCB-D-002768             PIC 9(04) VALUE 9376.
               10  SCB-DESC-002768          PIC X(18) VALUE 'DESC-035984'.
               10  SCB-E-002769             OCCURS 3 TIMES.
                   15  SCB-EK-002769        PIC X(10).
                   15  SCB-EV-002769        PIC 9(06) COMP.
               10  SCB-A-002770             PIC 9(09) COMP VALUE 2770.
               10  SCB-B-002771             PIC S9(07)V99 COMP-3 VALUE +0002771.
               10  SCB-C-002772             PIC X(24) VALUE 'FIELD-002772-ALPHA'.
           05  SCB-ALT-0042 REDEFINES SCB-GRP-0252.
               10  SCB-ALT-FLAG-0042      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0042   PIC X(64).
           05  SCB-GRP-0253.
               10  SCB-D-002773             PIC 9(04) VALUE 9411.
               10  SCB-DESC-002773          PIC X(18) VALUE 'DESC-036049'.
               10  SCB-E-002774             OCCURS 4 TIMES.
                   15  SCB-EK-002774        PIC X(10).
                   15  SCB-EV-002774        PIC 9(06) COMP.
               10  SCB-A-002775             PIC 9(09) COMP VALUE 2775.
               10  SCB-B-002776             PIC S9(07)V99 COMP-3 VALUE +0002776.
               10  SCB-C-002777             PIC X(24) VALUE 'FIELD-002777-ALPHA'.
               10  SCB-D-002778             PIC 9(04) VALUE 9446.
               10  SCB-DESC-002778          PIC X(18) VALUE 'DESC-036114'.
               10  SCB-E-002779             OCCURS 5 TIMES.
                   15  SCB-EK-002779        PIC X(10).
                   15  SCB-EV-002779        PIC 9(06) COMP.
               10  SCB-A-002780             PIC 9(09) COMP VALUE 2780.
               10  SCB-B-002781             PIC S9(07)V99 COMP-3 VALUE +0002781.
           05  SCB-GRP-0254.
               10  SCB-C-002782             PIC X(24) VALUE 'FIELD-002782-ALPHA'.
               10  SCB-D-002783             PIC 9(04) VALUE 9481.
               10  SCB-DESC-002783          PIC X(18) VALUE 'DESC-036179'.
               10  SCB-E-002784             OCCURS 2 TIMES.
                   15  SCB-EK-002784        PIC X(10).
                   15  SCB-EV-002784        PIC 9(06) COMP.
               10  SCB-A-002785             PIC 9(09) COMP VALUE 2785.
               10  SCB-B-002786             PIC S9(07)V99 COMP-3 VALUE +0002786.
               10  SCB-C-002787             PIC X(24) VALUE 'FIELD-002787-ALPHA'.
               10  SCB-D-002788             PIC 9(04) VALUE 9516.
               10  SCB-DESC-002788          PIC X(18) VALUE 'DESC-036244'.
               10  SCB-E-002789             OCCURS 3 TIMES.
                   15  SCB-EK-002789        PIC X(10).
                   15  SCB-EV-002789        PIC 9(06) COMP.
               10  SCB-A-002790             PIC 9(09) COMP VALUE 2790.
               10  SCB-B-002791             PIC S9(07)V99 COMP-3 VALUE +0002791.
           05  SCB-GRP-0255.
               10  SCB-C-002792             PIC X(24) VALUE 'FIELD-002792-ALPHA'.
               10  SCB-D-002793             PIC 9(04) VALUE 9551.
               10  SCB-DESC-002793          PIC X(18) VALUE 'DESC-036309'.
               10  SCB-E-002794             OCCURS 4 TIMES.
                   15  SCB-EK-002794        PIC X(10).
                   15  SCB-EV-002794        PIC 9(06) COMP.
               10  SCB-A-002795             PIC 9(09) COMP VALUE 2795.
               10  SCB-B-002796             PIC S9(07)V99 COMP-3 VALUE +0002796.
               10  SCB-C-002797             PIC X(24) VALUE 'FIELD-002797-ALPHA'.
               10  SCB-D-002798             PIC 9(04) VALUE 9586.
               10  SCB-DESC-002798          PIC X(18) VALUE 'DESC-036374'.
               10  SCB-E-002799             OCCURS 5 TIMES.
                   15  SCB-EK-002799        PIC X(10).
                   15  SCB-EV-002799        PIC 9(06) COMP.
               10  SCB-A-002800             PIC 9(09) COMP VALUE 2800.
               10  SCB-B-002801             PIC S9(07)V99 COMP-3 VALUE +0002801.
               10  SCB-C-002802             PIC X(24) VALUE 'FIELD-002802-ALPHA'.
           05  SCB-GRP-0256.
               10  SCB-D-002803             PIC 9(04) VALUE 9621.
               10  SCB-DESC-002803          PIC X(18) VALUE 'DESC-036439'.
               10  SCB-E-002804             OCCURS 2 TIMES.
                   15  SCB-EK-002804        PIC X(10).
                   15  SCB-EV-002804        PIC 9(06) COMP.
               10  SCB-A-002805             PIC 9(09) COMP VALUE 2805.
               10  SCB-B-002806             PIC S9(07)V99 COMP-3 VALUE +0002806.
               10  SCB-C-002807             PIC X(24) VALUE 'FIELD-002807-ALPHA'.
               10  SCB-D-002808             PIC 9(04) VALUE 9656.
               10  SCB-DESC-002808          PIC X(18) VALUE 'DESC-036504'.
               10  SCB-E-002809             OCCURS 3 TIMES.
                   15  SCB-EK-002809        PIC X(10).
                   15  SCB-EV-002809        PIC 9(06) COMP.
               10  SCB-A-002810             PIC 9(09) COMP VALUE 2810.
               10  SCB-B-002811             PIC S9(07)V99 COMP-3 VALUE +0002811.
               10  SCB-C-002812             PIC X(24) VALUE 'FIELD-002812-ALPHA'.
               10  SCB-D-002813             PIC 9(04) VALUE 9691.
               10  SCB-DESC-002813          PIC X(18) VALUE 'DESC-036569'.
               10  SCB-E-002814             OCCURS 4 TIMES.
                   15  SCB-EK-002814        PIC X(10).
                   15  SCB-EV-002814        PIC 9(06) COMP.
           05  SCB-GRP-0257.
               10  SCB-A-002815             PIC 9(09) COMP VALUE 2815.
               10  SCB-B-002816             PIC S9(07)V99 COMP-3 VALUE +0002816.
               10  SCB-C-002817             PIC X(24) VALUE 'FIELD-002817-ALPHA'.
               10  SCB-D-002818             PIC 9(04) VALUE 9726.
               10  SCB-DESC-002818          PIC X(18) VALUE 'DESC-036634'.
               10  SCB-E-002819             OCCURS 5 TIMES.
                   15  SCB-EK-002819        PIC X(10).
                   15  SCB-EV-002819        PIC 9(06) COMP.
               10  SCB-A-002820             PIC 9(09) COMP VALUE 2820.
               10  SCB-B-002821             PIC S9(07)V99 COMP-3 VALUE +0002821.
               10  SCB-C-002822             PIC X(24) VALUE 'FIELD-002822-ALPHA'.
               10  SCB-D-002823             PIC 9(04) VALUE 9761.
               10  SCB-DESC-002823          PIC X(18) VALUE 'DESC-036699'.
               10  SCB-E-002824             OCCURS 2 TIMES.
                   15  SCB-EK-002824        PIC X(10).
                   15  SCB-EV-002824        PIC 9(06) COMP.
               10  SCB-A-002825             PIC 9(09) COMP VALUE 2825.
               10  SCB-B-002826             PIC S9(07)V99 COMP-3 VALUE +0002826.
               10  SCB-C-002827             PIC X(24) VALUE 'FIELD-002827-ALPHA'.
           05  SCB-GRP-0258.
               10  SCB-D-002828             PIC 9(04) VALUE 9796.
               10  SCB-DESC-002828          PIC X(18) VALUE 'DESC-036764'.
               10  SCB-E-002829             OCCURS 3 TIMES.
                   15  SCB-EK-002829        PIC X(10).
                   15  SCB-EV-002829        PIC 9(06) COMP.
               10  SCB-A-002830             PIC 9(09) COMP VALUE 2830.
               10  SCB-B-002831             PIC S9(07)V99 COMP-3 VALUE +0002831.
               10  SCB-C-002832             PIC X(24) VALUE 'FIELD-002832-ALPHA'.
               10  SCB-D-002833             PIC 9(04) VALUE 9831.
               10  SCB-DESC-002833          PIC X(18) VALUE 'DESC-036829'.
               10  SCB-E-002834             OCCURS 4 TIMES.
                   15  SCB-EK-002834        PIC X(10).
                   15  SCB-EV-002834        PIC 9(06) COMP.
               10  SCB-A-002835             PIC 9(09) COMP VALUE 2835.
               10  SCB-B-002836             PIC S9(07)V99 COMP-3 VALUE +0002836.
               10  SCB-C-002837             PIC X(24) VALUE 'FIELD-002837-ALPHA'.
               10  SCB-D-002838             PIC 9(04) VALUE 9866.
               10  SCB-DESC-002838          PIC X(18) VALUE 'DESC-036894'.
               10  SCB-E-002839             OCCURS 5 TIMES.
                   15  SCB-EK-002839        PIC X(10).
                   15  SCB-EV-002839        PIC 9(06) COMP.
               10  SCB-A-002840             PIC 9(09) COMP VALUE 2840.
               10  SCB-B-002841             PIC S9(07)V99 COMP-3 VALUE +0002841.
           05  SCB-ALT-0043 REDEFINES SCB-GRP-0258.
               10  SCB-ALT-FLAG-0043      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0043   PIC X(64).
           05  SCB-GRP-0259.
               10  SCB-C-002842             PIC X(24) VALUE 'FIELD-002842-ALPHA'.
               10  SCB-D-002843             PIC 9(04) VALUE 9901.
               10  SCB-DESC-002843          PIC X(18) VALUE 'DESC-036959'.
               10  SCB-E-002844             OCCURS 2 TIMES.
                   15  SCB-EK-002844        PIC X(10).
                   15  SCB-EV-002844        PIC 9(06) COMP.
               10  SCB-A-002845             PIC 9(09) COMP VALUE 2845.
               10  SCB-B-002846             PIC S9(07)V99 COMP-3 VALUE +0002846.
               10  SCB-C-002847             PIC X(24) VALUE 'FIELD-002847-ALPHA'.
               10  SCB-D-002848             PIC 9(04) VALUE 9936.
               10  SCB-DESC-002848          PIC X(18) VALUE 'DESC-037024'.
               10  SCB-E-002849             OCCURS 3 TIMES.
                   15  SCB-EK-002849        PIC X(10).
                   15  SCB-EV-002849        PIC 9(06) COMP.
           05  SCB-GRP-0260.
               10  SCB-A-002850             PIC 9(09) COMP VALUE 2850.
               10  SCB-B-002851             PIC S9(07)V99 COMP-3 VALUE +0002851.
               10  SCB-C-002852             PIC X(24) VALUE 'FIELD-002852-ALPHA'.
               10  SCB-D-002853             PIC 9(04) VALUE 9971.
               10  SCB-DESC-002853          PIC X(18) VALUE 'DESC-037089'.
               10  SCB-E-002854             OCCURS 4 TIMES.
                   15  SCB-EK-002854        PIC X(10).
                   15  SCB-EV-002854        PIC 9(06) COMP.
               10  SCB-A-002855             PIC 9(09) COMP VALUE 2855.
               10  SCB-B-002856             PIC S9(07)V99 COMP-3 VALUE +0002856.
               10  SCB-C-002857             PIC X(24) VALUE 'FIELD-002857-ALPHA'.
               10  SCB-D-002858             PIC 9(04) VALUE 6.
               10  SCB-DESC-002858          PIC X(18) VALUE 'DESC-037154'.
           05  SCB-GRP-0261.
               10  SCB-E-002859             OCCURS 5 TIMES.
                   15  SCB-EK-002859        PIC X(10).
                   15  SCB-EV-002859        PIC 9(06) COMP.
               10  SCB-A-002860             PIC 9(09) COMP VALUE 2860.
               10  SCB-B-002861             PIC S9(07)V99 COMP-3 VALUE +0002861.
               10  SCB-C-002862             PIC X(24) VALUE 'FIELD-002862-ALPHA'.
               10  SCB-D-002863             PIC 9(04) VALUE 41.
               10  SCB-DESC-002863          PIC X(18) VALUE 'DESC-037219'.
               10  SCB-E-002864             OCCURS 2 TIMES.
                   15  SCB-EK-002864        PIC X(10).
                   15  SCB-EV-002864        PIC 9(06) COMP.
               10  SCB-A-002865             PIC 9(09) COMP VALUE 2865.
               10  SCB-B-002866             PIC S9(07)V99 COMP-3 VALUE +0002866.
               10  SCB-C-002867             PIC X(24) VALUE 'FIELD-002867-ALPHA'.
               10  SCB-D-002868             PIC 9(04) VALUE 76.
               10  SCB-DESC-002868          PIC X(18) VALUE 'DESC-037284'.
           05  SCB-GRP-0262.
               10  SCB-E-002869             OCCURS 3 TIMES.
                   15  SCB-EK-002869        PIC X(10).
                   15  SCB-EV-002869        PIC 9(06) COMP.
               10  SCB-A-002870             PIC 9(09) COMP VALUE 2870.
               10  SCB-B-002871             PIC S9(07)V99 COMP-3 VALUE +0002871.
               10  SCB-C-002872             PIC X(24) VALUE 'FIELD-002872-ALPHA'.
               10  SCB-D-002873             PIC 9(04) VALUE 111.
               10  SCB-DESC-002873          PIC X(18) VALUE 'DESC-037349'.
               10  SCB-E-002874             OCCURS 4 TIMES.
                   15  SCB-EK-002874        PIC X(10).
                   15  SCB-EV-002874        PIC 9(06) COMP.
               10  SCB-A-002875             PIC 9(09) COMP VALUE 2875.
               10  SCB-B-002876             PIC S9(07)V99 COMP-3 VALUE +0002876.
               10  SCB-C-002877             PIC X(24) VALUE 'FIELD-002877-ALPHA'.
               10  SCB-D-002878             PIC 9(04) VALUE 146.
               10  SCB-DESC-002878          PIC X(18) VALUE 'DESC-037414'.
               10  SCB-E-002879             OCCURS 5 TIMES.
                   15  SCB-EK-002879        PIC X(10).
                   15  SCB-EV-002879        PIC 9(06) COMP.
           05  SCB-GRP-0263.
               10  SCB-A-002880             PIC 9(09) COMP VALUE 2880.
               10  SCB-B-002881             PIC S9(07)V99 COMP-3 VALUE +0002881.
               10  SCB-C-002882             PIC X(24) VALUE 'FIELD-002882-ALPHA'.
               10  SCB-D-002883             PIC 9(04) VALUE 181.
               10  SCB-DESC-002883          PIC X(18) VALUE 'DESC-037479'.
               10  SCB-E-002884             OCCURS 2 TIMES.
                   15  SCB-EK-002884        PIC X(10).
                   15  SCB-EV-002884        PIC 9(06) COMP.
               10  SCB-A-002885             PIC 9(09) COMP VALUE 2885.
               10  SCB-B-002886             PIC S9(07)V99 COMP-3 VALUE +0002886.
               10  SCB-C-002887             PIC X(24) VALUE 'FIELD-002887-ALPHA'.
               10  SCB-D-002888             PIC 9(04) VALUE 216.
               10  SCB-DESC-002888          PIC X(18) VALUE 'DESC-037544'.
               10  SCB-E-002889             OCCURS 3 TIMES.
                   15  SCB-EK-002889        PIC X(10).
                   15  SCB-EV-002889        PIC 9(06) COMP.
               10  SCB-A-002890             PIC 9(09) COMP VALUE 2890.
               10  SCB-B-002891             PIC S9(07)V99 COMP-3 VALUE +0002891.
           05  SCB-GRP-0264.
               10  SCB-C-002892             PIC X(24) VALUE 'FIELD-002892-ALPHA'.
               10  SCB-D-002893             PIC 9(04) VALUE 251.
               10  SCB-DESC-002893          PIC X(18) VALUE 'DESC-037609'.
               10  SCB-E-002894             OCCURS 4 TIMES.
                   15  SCB-EK-002894        PIC X(10).
                   15  SCB-EV-002894        PIC 9(06) COMP.
               10  SCB-A-002895             PIC 9(09) COMP VALUE 2895.
               10  SCB-B-002896             PIC S9(07)V99 COMP-3 VALUE +0002896.
               10  SCB-C-002897             PIC X(24) VALUE 'FIELD-002897-ALPHA'.
               10  SCB-D-002898             PIC 9(04) VALUE 286.
               10  SCB-DESC-002898          PIC X(18) VALUE 'DESC-037674'.
               10  SCB-E-002899             OCCURS 5 TIMES.
                   15  SCB-EK-002899        PIC X(10).
                   15  SCB-EV-002899        PIC 9(06) COMP.
               10  SCB-A-002900             PIC 9(09) COMP VALUE 2900.
               10  SCB-B-002901             PIC S9(07)V99 COMP-3 VALUE +0002901.
               10  SCB-C-002902             PIC X(24) VALUE 'FIELD-002902-ALPHA'.
               10  SCB-D-002903             PIC 9(04) VALUE 321.
               10  SCB-DESC-002903          PIC X(18) VALUE 'DESC-037739'.
               10  SCB-E-002904             OCCURS 2 TIMES.
                   15  SCB-EK-002904        PIC X(10).
                   15  SCB-EV-002904        PIC 9(06) COMP.
           05  SCB-ALT-0044 REDEFINES SCB-GRP-0264.
               10  SCB-ALT-FLAG-0044      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0044   PIC X(64).
           05  SCB-GRP-0265.
               10  SCB-A-002905             PIC 9(09) COMP VALUE 2905.
               10  SCB-B-002906             PIC S9(07)V99 COMP-3 VALUE +0002906.
               10  SCB-C-002907             PIC X(24) VALUE 'FIELD-002907-ALPHA'.
               10  SCB-D-002908             PIC 9(04) VALUE 356.
               10  SCB-DESC-002908          PIC X(18) VALUE 'DESC-037804'.
               10  SCB-E-002909             OCCURS 3 TIMES.
                   15  SCB-EK-002909        PIC X(10).
                   15  SCB-EV-002909        PIC 9(06) COMP.
               10  SCB-A-002910             PIC 9(09) COMP VALUE 2910.
               10  SCB-B-002911             PIC S9(07)V99 COMP-3 VALUE +0002911.
               10  SCB-C-002912             PIC X(24) VALUE 'FIELD-002912-ALPHA'.
               10  SCB-D-002913             PIC 9(04) VALUE 391.
               10  SCB-DESC-002913          PIC X(18) VALUE 'DESC-037869'.
               10  SCB-E-002914             OCCURS 4 TIMES.
                   15  SCB-EK-002914        PIC X(10).
                   15  SCB-EV-002914        PIC 9(06) COMP.
               10  SCB-A-002915             PIC 9(09) COMP VALUE 2915.
               10  SCB-B-002916             PIC S9(07)V99 COMP-3 VALUE +0002916.
               10  SCB-C-002917             PIC X(24) VALUE 'FIELD-002917-ALPHA'.
               10  SCB-D-002918             PIC 9(04) VALUE 426.
               10  SCB-DESC-002918          PIC X(18) VALUE 'DESC-037934'.
           05  SCB-GRP-0266.
               10  SCB-E-002919             OCCURS 5 TIMES.
                   15  SCB-EK-002919        PIC X(10).
                   15  SCB-EV-002919        PIC 9(06) COMP.
               10  SCB-A-002920             PIC 9(09) COMP VALUE 2920.
               10  SCB-B-002921             PIC S9(07)V99 COMP-3 VALUE +0002921.
               10  SCB-C-002922             PIC X(24) VALUE 'FIELD-002922-ALPHA'.
               10  SCB-D-002923             PIC 9(04) VALUE 461.
               10  SCB-DESC-002923          PIC X(18) VALUE 'DESC-037999'.
               10  SCB-E-002924             OCCURS 2 TIMES.
                   15  SCB-EK-002924        PIC X(10).
                   15  SCB-EV-002924        PIC 9(06) COMP.
               10  SCB-A-002925             PIC 9(09) COMP VALUE 2925.
               10  SCB-B-002926             PIC S9(07)V99 COMP-3 VALUE +0002926.
           05  SCB-GRP-0267.
               10  SCB-C-002927             PIC X(24) VALUE 'FIELD-002927-ALPHA'.
               10  SCB-D-002928             PIC 9(04) VALUE 496.
               10  SCB-DESC-002928          PIC X(18) VALUE 'DESC-038064'.
               10  SCB-E-002929             OCCURS 3 TIMES.
                   15  SCB-EK-002929        PIC X(10).
                   15  SCB-EV-002929        PIC 9(06) COMP.
               10  SCB-A-002930             PIC 9(09) COMP VALUE 2930.
               10  SCB-B-002931             PIC S9(07)V99 COMP-3 VALUE +0002931.
               10  SCB-C-002932             PIC X(24) VALUE 'FIELD-002932-ALPHA'.
               10  SCB-D-002933             PIC 9(04) VALUE 531.
               10  SCB-DESC-002933          PIC X(18) VALUE 'DESC-038129'.
               10  SCB-E-002934             OCCURS 4 TIMES.
                   15  SCB-EK-002934        PIC X(10).
                   15  SCB-EV-002934        PIC 9(06) COMP.
               10  SCB-A-002935             PIC 9(09) COMP VALUE 2935.
           05  SCB-GRP-0268.
               10  SCB-B-002936             PIC S9(07)V99 COMP-3 VALUE +0002936.
               10  SCB-C-002937             PIC X(24) VALUE 'FIELD-002937-ALPHA'.
               10  SCB-D-002938             PIC 9(04) VALUE 566.
               10  SCB-DESC-002938          PIC X(18) VALUE 'DESC-038194'.
               10  SCB-E-002939             OCCURS 5 TIMES.
                   15  SCB-EK-002939        PIC X(10).
                   15  SCB-EV-002939        PIC 9(06) COMP.
               10  SCB-A-002940             PIC 9(09) COMP VALUE 2940.
               10  SCB-B-002941             PIC S9(07)V99 COMP-3 VALUE +0002941.
               10  SCB-C-002942             PIC X(24) VALUE 'FIELD-002942-ALPHA'.
               10  SCB-D-002943             PIC 9(04) VALUE 601.
               10  SCB-DESC-002943          PIC X(18) VALUE 'DESC-038259'.
               10  SCB-E-002944             OCCURS 2 TIMES.
                   15  SCB-EK-002944        PIC X(10).
                   15  SCB-EV-002944        PIC 9(06) COMP.
               10  SCB-A-002945             PIC 9(09) COMP VALUE 2945.
           05  SCB-GRP-0269.
               10  SCB-B-002946             PIC S9(07)V99 COMP-3 VALUE +0002946.
               10  SCB-C-002947             PIC X(24) VALUE 'FIELD-002947-ALPHA'.
               10  SCB-D-002948             PIC 9(04) VALUE 636.
               10  SCB-DESC-002948          PIC X(18) VALUE 'DESC-038324'.
               10  SCB-E-002949             OCCURS 3 TIMES.
                   15  SCB-EK-002949        PIC X(10).
                   15  SCB-EV-002949        PIC 9(06) COMP.
               10  SCB-A-002950             PIC 9(09) COMP VALUE 2950.
               10  SCB-B-002951             PIC S9(07)V99 COMP-3 VALUE +0002951.
               10  SCB-C-002952             PIC X(24) VALUE 'FIELD-002952-ALPHA'.
               10  SCB-D-002953             PIC 9(04) VALUE 671.
               10  SCB-DESC-002953          PIC X(18) VALUE 'DESC-038389'.
               10  SCB-E-002954             OCCURS 4 TIMES.
                   15  SCB-EK-002954        PIC X(10).
                   15  SCB-EV-002954        PIC 9(06) COMP.
               10  SCB-A-002955             PIC 9(09) COMP VALUE 2955.
               10  SCB-B-002956             PIC S9(07)V99 COMP-3 VALUE +0002956.
           05  SCB-GRP-0270.
               10  SCB-C-002957             PIC X(24) VALUE 'FIELD-002957-ALPHA'.
               10  SCB-D-002958             PIC 9(04) VALUE 706.
               10  SCB-DESC-002958          PIC X(18) VALUE 'DESC-038454'.
               10  SCB-E-002959             OCCURS 5 TIMES.
                   15  SCB-EK-002959        PIC X(10).
                   15  SCB-EV-002959        PIC 9(06) COMP.
               10  SCB-A-002960             PIC 9(09) COMP VALUE 2960.
               10  SCB-B-002961             PIC S9(07)V99 COMP-3 VALUE +0002961.
               10  SCB-C-002962             PIC X(24) VALUE 'FIELD-002962-ALPHA'.
               10  SCB-D-002963             PIC 9(04) VALUE 741.
               10  SCB-DESC-002963          PIC X(18) VALUE 'DESC-038519'.
               10  SCB-E-002964             OCCURS 2 TIMES.
                   15  SCB-EK-002964        PIC X(10).
                   15  SCB-EV-002964        PIC 9(06) COMP.
               10  SCB-A-002965             PIC 9(09) COMP VALUE 2965.
               10  SCB-B-002966             PIC S9(07)V99 COMP-3 VALUE +0002966.
               10  SCB-C-002967             PIC X(24) VALUE 'FIELD-002967-ALPHA'.
               10  SCB-D-002968             PIC 9(04) VALUE 776.
               10  SCB-DESC-002968          PIC X(18) VALUE 'DESC-038584'.
           05  SCB-ALT-0045 REDEFINES SCB-GRP-0270.
               10  SCB-ALT-FLAG-0045      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0045   PIC X(64).
           05  SCB-GRP-0271.
               10  SCB-E-002969             OCCURS 3 TIMES.
                   15  SCB-EK-002969        PIC X(10).
                   15  SCB-EV-002969        PIC 9(06) COMP.
               10  SCB-A-002970             PIC 9(09) COMP VALUE 2970.
               10  SCB-B-002971             PIC S9(07)V99 COMP-3 VALUE +0002971.
               10  SCB-C-002972             PIC X(24) VALUE 'FIELD-002972-ALPHA'.
               10  SCB-D-002973             PIC 9(04) VALUE 811.
               10  SCB-DESC-002973          PIC X(18) VALUE 'DESC-038649'.
               10  SCB-E-002974             OCCURS 4 TIMES.
                   15  SCB-EK-002974        PIC X(10).
                   15  SCB-EV-002974        PIC 9(06) COMP.
               10  SCB-A-002975             PIC 9(09) COMP VALUE 2975.
               10  SCB-B-002976             PIC S9(07)V99 COMP-3 VALUE +0002976.
               10  SCB-C-002977             PIC X(24) VALUE 'FIELD-002977-ALPHA'.
               10  SCB-D-002978             PIC 9(04) VALUE 846.
               10  SCB-DESC-002978          PIC X(18) VALUE 'DESC-038714'.
               10  SCB-E-002979             OCCURS 5 TIMES.
                   15  SCB-EK-002979        PIC X(10).
                   15  SCB-EV-002979        PIC 9(06) COMP.
               10  SCB-A-002980             PIC 9(09) COMP VALUE 2980.
               10  SCB-B-002981             PIC S9(07)V99 COMP-3 VALUE +0002981.
           05  SCB-GRP-0272.
               10  SCB-C-002982             PIC X(24) VALUE 'FIELD-002982-ALPHA'.
               10  SCB-D-002983             PIC 9(04) VALUE 881.
               10  SCB-DESC-002983          PIC X(18) VALUE 'DESC-038779'.
               10  SCB-E-002984             OCCURS 2 TIMES.
                   15  SCB-EK-002984        PIC X(10).
                   15  SCB-EV-002984        PIC 9(06) COMP.
               10  SCB-A-002985             PIC 9(09) COMP VALUE 2985.
               10  SCB-B-002986             PIC S9(07)V99 COMP-3 VALUE +0002986.
               10  SCB-C-002987             PIC X(24) VALUE 'FIELD-002987-ALPHA'.
               10  SCB-D-002988             PIC 9(04) VALUE 916.
               10  SCB-DESC-002988          PIC X(18) VALUE 'DESC-038844'.
               10  SCB-E-002989             OCCURS 3 TIMES.
                   15  SCB-EK-002989        PIC X(10).
                   15  SCB-EV-002989        PIC 9(06) COMP.
               10  SCB-A-002990             PIC 9(09) COMP VALUE 2990.
               10  SCB-B-002991             PIC S9(07)V99 COMP-3 VALUE +0002991.
               10  SCB-C-002992             PIC X(24) VALUE 'FIELD-002992-ALPHA'.
               10  SCB-D-002993             PIC 9(04) VALUE 951.
               10  SCB-DESC-002993          PIC X(18) VALUE 'DESC-038909'.
               10  SCB-E-002994             OCCURS 4 TIMES.
                   15  SCB-EK-002994        PIC X(10).
                   15  SCB-EV-002994        PIC 9(06) COMP.
               10  SCB-A-002995             PIC 9(09) COMP VALUE 2995.
           05  SCB-GRP-0273.
               10  SCB-B-002996             PIC S9(07)V99 COMP-3 VALUE +0002996.
               10  SCB-C-002997             PIC X(24) VALUE 'FIELD-002997-ALPHA'.
               10  SCB-D-002998             PIC 9(04) VALUE 986.
               10  SCB-DESC-002998          PIC X(18) VALUE 'DESC-038974'.
               10  SCB-E-002999             OCCURS 5 TIMES.
                   15  SCB-EK-002999        PIC X(10).
                   15  SCB-EV-002999        PIC 9(06) COMP.
               10  SCB-A-003000             PIC 9(09) COMP VALUE 3000.
               10  SCB-B-003001             PIC S9(07)V99 COMP-3 VALUE +0003001.
               10  SCB-C-003002             PIC X(24) VALUE 'FIELD-003002-ALPHA'.
               10  SCB-D-003003             PIC 9(04) VALUE 1021.
               10  SCB-DESC-003003          PIC X(18) VALUE 'DESC-039039'.
           05  SCB-GRP-0274.
               10  SCB-E-003004             OCCURS 2 TIMES.
                   15  SCB-EK-003004        PIC X(10).
                   15  SCB-EV-003004        PIC 9(06) COMP.
               10  SCB-A-003005             PIC 9(09) COMP VALUE 3005.
               10  SCB-B-003006             PIC S9(07)V99 COMP-3 VALUE +0003006.
               10  SCB-C-003007             PIC X(24) VALUE 'FIELD-003007-ALPHA'.
               10  SCB-D-003008             PIC 9(04) VALUE 1056.
               10  SCB-DESC-003008          PIC X(18) VALUE 'DESC-039104'.
               10  SCB-E-003009             OCCURS 3 TIMES.
                   15  SCB-EK-003009        PIC X(10).
                   15  SCB-EV-003009        PIC 9(06) COMP.
               10  SCB-A-003010             PIC 9(09) COMP VALUE 3010.
               10  SCB-B-003011             PIC S9(07)V99 COMP-3 VALUE +0003011.
               10  SCB-C-003012             PIC X(24) VALUE 'FIELD-003012-ALPHA'.
           05  SCB-GRP-0275.
               10  SCB-D-003013             PIC 9(04) VALUE 1091.
               10  SCB-DESC-003013          PIC X(18) VALUE 'DESC-039169'.
               10  SCB-E-003014             OCCURS 4 TIMES.
                   15  SCB-EK-003014        PIC X(10).
                   15  SCB-EV-003014        PIC 9(06) COMP.
               10  SCB-A-003015             PIC 9(09) COMP VALUE 3015.
               10  SCB-B-003016             PIC S9(07)V99 COMP-3 VALUE +0003016.
               10  SCB-C-003017             PIC X(24) VALUE 'FIELD-003017-ALPHA'.
               10  SCB-D-003018             PIC 9(04) VALUE 1126.
               10  SCB-DESC-003018          PIC X(18) VALUE 'DESC-039234'.
               10  SCB-E-003019             OCCURS 5 TIMES.
                   15  SCB-EK-003019        PIC X(10).
                   15  SCB-EV-003019        PIC 9(06) COMP.
               10  SCB-A-003020             PIC 9(09) COMP VALUE 3020.
               10  SCB-B-003021             PIC S9(07)V99 COMP-3 VALUE +0003021.
               10  SCB-C-003022             PIC X(24) VALUE 'FIELD-003022-ALPHA'.
           05  SCB-GRP-0276.
               10  SCB-D-003023             PIC 9(04) VALUE 1161.
               10  SCB-DESC-003023          PIC X(18) VALUE 'DESC-039299'.
               10  SCB-E-003024             OCCURS 2 TIMES.
                   15  SCB-EK-003024        PIC X(10).
                   15  SCB-EV-003024        PIC 9(06) COMP.
               10  SCB-A-003025             PIC 9(09) COMP VALUE 3025.
               10  SCB-B-003026             PIC S9(07)V99 COMP-3 VALUE +0003026.
               10  SCB-C-003027             PIC X(24) VALUE 'FIELD-003027-ALPHA'.
               10  SCB-D-003028             PIC 9(04) VALUE 1196.
               10  SCB-DESC-003028          PIC X(18) VALUE 'DESC-039364'.
               10  SCB-E-003029             OCCURS 3 TIMES.
                   15  SCB-EK-003029        PIC X(10).
                   15  SCB-EV-003029        PIC 9(06) COMP.
               10  SCB-A-003030             PIC 9(09) COMP VALUE 3030.
               10  SCB-B-003031             PIC S9(07)V99 COMP-3 VALUE +0003031.
               10  SCB-C-003032             PIC X(24) VALUE 'FIELD-003032-ALPHA'.
               10  SCB-D-003033             PIC 9(04) VALUE 1231.
               10  SCB-DESC-003033          PIC X(18) VALUE 'DESC-039429'.
           05  SCB-ALT-0046 REDEFINES SCB-GRP-0276.
               10  SCB-ALT-FLAG-0046      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0046   PIC X(64).
           05  SCB-GRP-0277.
               10  SCB-E-003034             OCCURS 4 TIMES.
                   15  SCB-EK-003034        PIC X(10).
                   15  SCB-EV-003034        PIC 9(06) COMP.
               10  SCB-A-003035             PIC 9(09) COMP VALUE 3035.
               10  SCB-B-003036             PIC S9(07)V99 COMP-3 VALUE +0003036.
               10  SCB-C-003037             PIC X(24) VALUE 'FIELD-003037-ALPHA'.
               10  SCB-D-003038             PIC 9(04) VALUE 1266.
               10  SCB-DESC-003038          PIC X(18) VALUE 'DESC-039494'.
               10  SCB-E-003039             OCCURS 5 TIMES.
                   15  SCB-EK-003039        PIC X(10).
                   15  SCB-EV-003039        PIC 9(06) COMP.
               10  SCB-A-003040             PIC 9(09) COMP VALUE 3040.
               10  SCB-B-003041             PIC S9(07)V99 COMP-3 VALUE +0003041.
               10  SCB-C-003042             PIC X(24) VALUE 'FIELD-003042-ALPHA'.
               10  SCB-D-003043             PIC 9(04) VALUE 1301.
               10  SCB-DESC-003043          PIC X(18) VALUE 'DESC-039559'.
               10  SCB-E-003044             OCCURS 2 TIMES.
                   15  SCB-EK-003044        PIC X(10).
                   15  SCB-EV-003044        PIC 9(06) COMP.
               10  SCB-A-003045             PIC 9(09) COMP VALUE 3045.
           05  SCB-GRP-0278.
               10  SCB-B-003046             PIC S9(07)V99 COMP-3 VALUE +0003046.
               10  SCB-C-003047             PIC X(24) VALUE 'FIELD-003047-ALPHA'.
               10  SCB-D-003048             PIC 9(04) VALUE 1336.
               10  SCB-DESC-003048          PIC X(18) VALUE 'DESC-039624'.
               10  SCB-E-003049             OCCURS 3 TIMES.
                   15  SCB-EK-003049        PIC X(10).
                   15  SCB-EV-003049        PIC 9(06) COMP.
               10  SCB-A-003050             PIC 9(09) COMP VALUE 3050.
               10  SCB-B-003051             PIC S9(07)V99 COMP-3 VALUE +0003051.
               10  SCB-C-003052             PIC X(24) VALUE 'FIELD-003052-ALPHA'.
               10  SCB-D-003053             PIC 9(04) VALUE 1371.
               10  SCB-DESC-003053          PIC X(18) VALUE 'DESC-039689'.
               10  SCB-E-003054             OCCURS 4 TIMES.
                   15  SCB-EK-003054        PIC X(10).
                   15  SCB-EV-003054        PIC 9(06) COMP.
               10  SCB-A-003055             PIC 9(09) COMP VALUE 3055.
               10  SCB-B-003056             PIC S9(07)V99 COMP-3 VALUE +0003056.
               10  SCB-C-003057             PIC X(24) VALUE 'FIELD-003057-ALPHA'.
               10  SCB-D-003058             PIC 9(04) VALUE 1406.
               10  SCB-DESC-003058          PIC X(18) VALUE 'DESC-039754'.
           05  SCB-GRP-0279.
               10  SCB-E-003059             OCCURS 5 TIMES.
                   15  SCB-EK-003059        PIC X(10).
                   15  SCB-EV-003059        PIC 9(06) COMP.
               10  SCB-A-003060             PIC 9(09) COMP VALUE 3060.
               10  SCB-B-003061             PIC S9(07)V99 COMP-3 VALUE +0003061.
               10  SCB-C-003062             PIC X(24) VALUE 'FIELD-003062-ALPHA'.
               10  SCB-D-003063             PIC 9(04) VALUE 1441.
               10  SCB-DESC-003063          PIC X(18) VALUE 'DESC-039819'.
               10  SCB-E-003064             OCCURS 2 TIMES.
                   15  SCB-EK-003064        PIC X(10).
                   15  SCB-EV-003064        PIC 9(06) COMP.
               10  SCB-A-003065             PIC 9(09) COMP VALUE 3065.
               10  SCB-B-003066             PIC S9(07)V99 COMP-3 VALUE +0003066.
               10  SCB-C-003067             PIC X(24) VALUE 'FIELD-003067-ALPHA'.
               10  SCB-D-003068             PIC 9(04) VALUE 1476.
               10  SCB-DESC-003068          PIC X(18) VALUE 'DESC-039884'.
               10  SCB-E-003069             OCCURS 3 TIMES.
                   15  SCB-EK-003069        PIC X(10).
                   15  SCB-EV-003069        PIC 9(06) COMP.
               10  SCB-A-003070             PIC 9(09) COMP VALUE 3070.
               10  SCB-B-003071             PIC S9(07)V99 COMP-3 VALUE +0003071.
               10  SCB-C-003072             PIC X(24) VALUE 'FIELD-003072-ALPHA'.
           05  SCB-GRP-0280.
               10  SCB-D-003073             PIC 9(04) VALUE 1511.
               10  SCB-DESC-003073          PIC X(18) VALUE 'DESC-039949'.
               10  SCB-E-003074             OCCURS 4 TIMES.
                   15  SCB-EK-003074        PIC X(10).
                   15  SCB-EV-003074        PIC 9(06) COMP.
               10  SCB-A-003075             PIC 9(09) COMP VALUE 3075.
               10  SCB-B-003076             PIC S9(07)V99 COMP-3 VALUE +0003076.
               10  SCB-C-003077             PIC X(24) VALUE 'FIELD-003077-ALPHA'.
               10  SCB-D-003078             PIC 9(04) VALUE 1546.
               10  SCB-DESC-003078          PIC X(18) VALUE 'DESC-040014'.
               10  SCB-E-003079             OCCURS 5 TIMES.
                   15  SCB-EK-003079        PIC X(10).
                   15  SCB-EV-003079        PIC 9(06) COMP.
               10  SCB-A-003080             PIC 9(09) COMP VALUE 3080.
           05  SCB-GRP-0281.
               10  SCB-B-003081             PIC S9(07)V99 COMP-3 VALUE +0003081.
               10  SCB-C-003082             PIC X(24) VALUE 'FIELD-003082-ALPHA'.
               10  SCB-D-003083             PIC 9(04) VALUE 1581.
               10  SCB-DESC-003083          PIC X(18) VALUE 'DESC-040079'.
               10  SCB-E-003084             OCCURS 2 TIMES.
                   15  SCB-EK-003084        PIC X(10).
                   15  SCB-EV-003084        PIC 9(06) COMP.
               10  SCB-A-003085             PIC 9(09) COMP VALUE 3085.
               10  SCB-B-003086             PIC S9(07)V99 COMP-3 VALUE +0003086.
               10  SCB-C-003087             PIC X(24) VALUE 'FIELD-003087-ALPHA'.
               10  SCB-D-003088             PIC 9(04) VALUE 1616.
               10  SCB-DESC-003088          PIC X(18) VALUE 'DESC-040144'.
               10  SCB-E-003089             OCCURS 3 TIMES.
                   15  SCB-EK-003089        PIC X(10).
                   15  SCB-EV-003089        PIC 9(06) COMP.
           05  SCB-GRP-0282.
               10  SCB-A-003090             PIC 9(09) COMP VALUE 3090.
               10  SCB-B-003091             PIC S9(07)V99 COMP-3 VALUE +0003091.
               10  SCB-C-003092             PIC X(24) VALUE 'FIELD-003092-ALPHA'.
               10  SCB-D-003093             PIC 9(04) VALUE 1651.
               10  SCB-DESC-003093          PIC X(18) VALUE 'DESC-040209'.
               10  SCB-E-003094             OCCURS 4 TIMES.
                   15  SCB-EK-003094        PIC X(10).
                   15  SCB-EV-003094        PIC 9(06) COMP.
               10  SCB-A-003095             PIC 9(09) COMP VALUE 3095.
               10  SCB-B-003096             PIC S9(07)V99 COMP-3 VALUE +0003096.
               10  SCB-C-003097             PIC X(24) VALUE 'FIELD-003097-ALPHA'.
               10  SCB-D-003098             PIC 9(04) VALUE 1686.
               10  SCB-DESC-003098          PIC X(18) VALUE 'DESC-040274'.
               10  SCB-E-003099             OCCURS 5 TIMES.
                   15  SCB-EK-003099        PIC X(10).
                   15  SCB-EV-003099        PIC 9(06) COMP.
           05  SCB-ALT-0047 REDEFINES SCB-GRP-0282.
               10  SCB-ALT-FLAG-0047      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0047   PIC X(64).
           05  SCB-GRP-0283.
               10  SCB-A-003100             PIC 9(09) COMP VALUE 3100.
               10  SCB-B-003101             PIC S9(07)V99 COMP-3 VALUE +0003101.
               10  SCB-C-003102             PIC X(24) VALUE 'FIELD-003102-ALPHA'.
               10  SCB-D-003103             PIC 9(04) VALUE 1721.
               10  SCB-DESC-003103          PIC X(18) VALUE 'DESC-040339'.
               10  SCB-E-003104             OCCURS 2 TIMES.
                   15  SCB-EK-003104        PIC X(10).
                   15  SCB-EV-003104        PIC 9(06) COMP.
               10  SCB-A-003105             PIC 9(09) COMP VALUE 3105.
               10  SCB-B-003106             PIC S9(07)V99 COMP-3 VALUE +0003106.
               10  SCB-C-003107             PIC X(24) VALUE 'FIELD-003107-ALPHA'.
               10  SCB-D-003108             PIC 9(04) VALUE 1756.
               10  SCB-DESC-003108          PIC X(18) VALUE 'DESC-040404'.
               10  SCB-E-003109             OCCURS 3 TIMES.
                   15  SCB-EK-003109        PIC X(10).
                   15  SCB-EV-003109        PIC 9(06) COMP.
               10  SCB-A-003110             PIC 9(09) COMP VALUE 3110.
           05  SCB-GRP-0284.
               10  SCB-B-003111             PIC S9(07)V99 COMP-3 VALUE +0003111.
               10  SCB-C-003112             PIC X(24) VALUE 'FIELD-003112-ALPHA'.
               10  SCB-D-003113             PIC 9(04) VALUE 1791.
               10  SCB-DESC-003113          PIC X(18) VALUE 'DESC-040469'.
               10  SCB-E-003114             OCCURS 4 TIMES.
                   15  SCB-EK-003114        PIC X(10).
                   15  SCB-EV-003114        PIC 9(06) COMP.
               10  SCB-A-003115             PIC 9(09) COMP VALUE 3115.
               10  SCB-B-003116             PIC S9(07)V99 COMP-3 VALUE +0003116.
               10  SCB-C-003117             PIC X(24) VALUE 'FIELD-003117-ALPHA'.
               10  SCB-D-003118             PIC 9(04) VALUE 1826.
               10  SCB-DESC-003118          PIC X(18) VALUE 'DESC-040534'.
               10  SCB-E-003119             OCCURS 5 TIMES.
                   15  SCB-EK-003119        PIC X(10).
                   15  SCB-EV-003119        PIC 9(06) COMP.
               10  SCB-A-003120             PIC 9(09) COMP VALUE 3120.
               10  SCB-B-003121             PIC S9(07)V99 COMP-3 VALUE +0003121.
               10  SCB-C-003122             PIC X(24) VALUE 'FIELD-003122-ALPHA'.
           05  SCB-GRP-0285.
               10  SCB-D-003123             PIC 9(04) VALUE 1861.
               10  SCB-DESC-003123          PIC X(18) VALUE 'DESC-040599'.
               10  SCB-E-003124             OCCURS 2 TIMES.
                   15  SCB-EK-003124        PIC X(10).
                   15  SCB-EV-003124        PIC 9(06) COMP.
               10  SCB-A-003125             PIC 9(09) COMP VALUE 3125.
               10  SCB-B-003126             PIC S9(07)V99 COMP-3 VALUE +0003126.
               10  SCB-C-003127             PIC X(24) VALUE 'FIELD-003127-ALPHA'.
               10  SCB-D-003128             PIC 9(04) VALUE 1896.
               10  SCB-DESC-003128          PIC X(18) VALUE 'DESC-040664'.
               10  SCB-E-003129             OCCURS 3 TIMES.
                   15  SCB-EK-003129        PIC X(10).
                   15  SCB-EV-003129        PIC 9(06) COMP.
               10  SCB-A-003130             PIC 9(09) COMP VALUE 3130.
               10  SCB-B-003131             PIC S9(07)V99 COMP-3 VALUE +0003131.
               10  SCB-C-003132             PIC X(24) VALUE 'FIELD-003132-ALPHA'.
               10  SCB-D-003133             PIC 9(04) VALUE 1931.
               10  SCB-DESC-003133          PIC X(18) VALUE 'DESC-040729'.
               10  SCB-E-003134             OCCURS 4 TIMES.
                   15  SCB-EK-003134        PIC X(10).
                   15  SCB-EV-003134        PIC 9(06) COMP.
               10  SCB-A-003135             PIC 9(09) COMP VALUE 3135.
           05  SCB-GRP-0286.
               10  SCB-B-003136             PIC S9(07)V99 COMP-3 VALUE +0003136.
               10  SCB-C-003137             PIC X(24) VALUE 'FIELD-003137-ALPHA'.
               10  SCB-D-003138             PIC 9(04) VALUE 1966.
               10  SCB-DESC-003138          PIC X(18) VALUE 'DESC-040794'.
               10  SCB-E-003139             OCCURS 5 TIMES.
                   15  SCB-EK-003139        PIC X(10).
                   15  SCB-EV-003139        PIC 9(06) COMP.
               10  SCB-A-003140             PIC 9(09) COMP VALUE 3140.
               10  SCB-B-003141             PIC S9(07)V99 COMP-3 VALUE +0003141.
               10  SCB-C-003142             PIC X(24) VALUE 'FIELD-003142-ALPHA'.
               10  SCB-D-003143             PIC 9(04) VALUE 2001.
               10  SCB-DESC-003143          PIC X(18) VALUE 'DESC-040859'.
               10  SCB-E-003144             OCCURS 2 TIMES.
                   15  SCB-EK-003144        PIC X(10).
                   15  SCB-EV-003144        PIC 9(06) COMP.
               10  SCB-A-003145             PIC 9(09) COMP VALUE 3145.
               10  SCB-B-003146             PIC S9(07)V99 COMP-3 VALUE +0003146.
               10  SCB-C-003147             PIC X(24) VALUE 'FIELD-003147-ALPHA'.
               10  SCB-D-003148             PIC 9(04) VALUE 2036.
               10  SCB-DESC-003148          PIC X(18) VALUE 'DESC-040924'.
               10  SCB-E-003149             OCCURS 3 TIMES.
                   15  SCB-EK-003149        PIC X(10).
                   15  SCB-EV-003149        PIC 9(06) COMP.
           05  SCB-GRP-0287.
               10  SCB-A-003150             PIC 9(09) COMP VALUE 3150.
               10  SCB-B-003151             PIC S9(07)V99 COMP-3 VALUE +0003151.
               10  SCB-C-003152             PIC X(24) VALUE 'FIELD-003152-ALPHA'.
               10  SCB-D-003153             PIC 9(04) VALUE 2071.
               10  SCB-DESC-003153          PIC X(18) VALUE 'DESC-040989'.
               10  SCB-E-003154             OCCURS 4 TIMES.
                   15  SCB-EK-003154        PIC X(10).
                   15  SCB-EV-003154        PIC 9(06) COMP.
               10  SCB-A-003155             PIC 9(09) COMP VALUE 3155.
               10  SCB-B-003156             PIC S9(07)V99 COMP-3 VALUE +0003156.
               10  SCB-C-003157             PIC X(24) VALUE 'FIELD-003157-ALPHA'.
           05  SCB-GRP-0288.
               10  SCB-D-003158             PIC 9(04) VALUE 2106.
               10  SCB-DESC-003158          PIC X(18) VALUE 'DESC-041054'.
               10  SCB-E-003159             OCCURS 5 TIMES.
                   15  SCB-EK-003159        PIC X(10).
                   15  SCB-EV-003159        PIC 9(06) COMP.
               10  SCB-A-003160             PIC 9(09) COMP VALUE 3160.
               10  SCB-B-003161             PIC S9(07)V99 COMP-3 VALUE +0003161.
               10  SCB-C-003162             PIC X(24) VALUE 'FIELD-003162-ALPHA'.
               10  SCB-D-003163             PIC 9(04) VALUE 2141.
               10  SCB-DESC-003163          PIC X(18) VALUE 'DESC-041119'.
               10  SCB-E-003164             OCCURS 2 TIMES.
                   15  SCB-EK-003164        PIC X(10).
                   15  SCB-EV-003164        PIC 9(06) COMP.
               10  SCB-A-003165             PIC 9(09) COMP VALUE 3165.
               10  SCB-B-003166             PIC S9(07)V99 COMP-3 VALUE +0003166.
           05  SCB-ALT-0048 REDEFINES SCB-GRP-0288.
               10  SCB-ALT-FLAG-0048      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0048   PIC X(64).
           05  SCB-GRP-0289.
               10  SCB-C-003167             PIC X(24) VALUE 'FIELD-003167-ALPHA'.
               10  SCB-D-003168             PIC 9(04) VALUE 2176.
               10  SCB-DESC-003168          PIC X(18) VALUE 'DESC-041184'.
               10  SCB-E-003169             OCCURS 3 TIMES.
                   15  SCB-EK-003169        PIC X(10).
                   15  SCB-EV-003169        PIC 9(06) COMP.
               10  SCB-A-003170             PIC 9(09) COMP VALUE 3170.
               10  SCB-B-003171             PIC S9(07)V99 COMP-3 VALUE +0003171.
               10  SCB-C-003172             PIC X(24) VALUE 'FIELD-003172-ALPHA'.
               10  SCB-D-003173             PIC 9(04) VALUE 2211.
               10  SCB-DESC-003173          PIC X(18) VALUE 'DESC-041249'.
               10  SCB-E-003174             OCCURS 4 TIMES.
                   15  SCB-EK-003174        PIC X(10).
                   15  SCB-EV-003174        PIC 9(06) COMP.
               10  SCB-A-003175             PIC 9(09) COMP VALUE 3175.
               10  SCB-B-003176             PIC S9(07)V99 COMP-3 VALUE +0003176.
           05  SCB-GRP-0290.
               10  SCB-C-003177             PIC X(24) VALUE 'FIELD-003177-ALPHA'.
               10  SCB-D-003178             PIC 9(04) VALUE 2246.
               10  SCB-DESC-003178          PIC X(18) VALUE 'DESC-041314'.
               10  SCB-E-003179             OCCURS 5 TIMES.
                   15  SCB-EK-003179        PIC X(10).
                   15  SCB-EV-003179        PIC 9(06) COMP.
               10  SCB-A-003180             PIC 9(09) COMP VALUE 3180.
               10  SCB-B-003181             PIC S9(07)V99 COMP-3 VALUE +0003181.
               10  SCB-C-003182             PIC X(24) VALUE 'FIELD-003182-ALPHA'.
               10  SCB-D-003183             PIC 9(04) VALUE 2281.
               10  SCB-DESC-003183          PIC X(18) VALUE 'DESC-041379'.
               10  SCB-E-003184             OCCURS 2 TIMES.
                   15  SCB-EK-003184        PIC X(10).
                   15  SCB-EV-003184        PIC 9(06) COMP.
               10  SCB-A-003185             PIC 9(09) COMP VALUE 3185.
               10  SCB-B-003186             PIC S9(07)V99 COMP-3 VALUE +0003186.
               10  SCB-C-003187             PIC X(24) VALUE 'FIELD-003187-ALPHA'.
           05  SCB-GRP-0291.
               10  SCB-D-003188             PIC 9(04) VALUE 2316.
               10  SCB-DESC-003188          PIC X(18) VALUE 'DESC-041444'.
               10  SCB-E-003189             OCCURS 3 TIMES.
                   15  SCB-EK-003189        PIC X(10).
                   15  SCB-EV-003189        PIC 9(06) COMP.
               10  SCB-A-003190             PIC 9(09) COMP VALUE 3190.
               10  SCB-B-003191             PIC S9(07)V99 COMP-3 VALUE +0003191.
               10  SCB-C-003192             PIC X(24) VALUE 'FIELD-003192-ALPHA'.
               10  SCB-D-003193             PIC 9(04) VALUE 2351.
               10  SCB-DESC-003193          PIC X(18) VALUE 'DESC-041509'.
               10  SCB-E-003194             OCCURS 4 TIMES.
                   15  SCB-EK-003194        PIC X(10).
                   15  SCB-EV-003194        PIC 9(06) COMP.
               10  SCB-A-003195             PIC 9(09) COMP VALUE 3195.
               10  SCB-B-003196             PIC S9(07)V99 COMP-3 VALUE +0003196.
               10  SCB-C-003197             PIC X(24) VALUE 'FIELD-003197-ALPHA'.
               10  SCB-D-003198             PIC 9(04) VALUE 2386.
               10  SCB-DESC-003198          PIC X(18) VALUE 'DESC-041574'.
               10  SCB-E-003199             OCCURS 5 TIMES.
                   15  SCB-EK-003199        PIC X(10).
                   15  SCB-EV-003199        PIC 9(06) COMP.
           05  SCB-GRP-0292.
               10  SCB-A-003200             PIC 9(09) COMP VALUE 3200.
               10  SCB-B-003201             PIC S9(07)V99 COMP-3 VALUE +0003201.
               10  SCB-C-003202             PIC X(24) VALUE 'FIELD-003202-ALPHA'.
               10  SCB-D-003203             PIC 9(04) VALUE 2421.
               10  SCB-DESC-003203          PIC X(18) VALUE 'DESC-041639'.
               10  SCB-E-003204             OCCURS 2 TIMES.
                   15  SCB-EK-003204        PIC X(10).
                   15  SCB-EV-003204        PIC 9(06) COMP.
               10  SCB-A-003205             PIC 9(09) COMP VALUE 3205.
               10  SCB-B-003206             PIC S9(07)V99 COMP-3 VALUE +0003206.
               10  SCB-C-003207             PIC X(24) VALUE 'FIELD-003207-ALPHA'.
               10  SCB-D-003208             PIC 9(04) VALUE 2456.
               10  SCB-DESC-003208          PIC X(18) VALUE 'DESC-041704'.
               10  SCB-E-003209             OCCURS 3 TIMES.
                   15  SCB-EK-003209        PIC X(10).
                   15  SCB-EV-003209        PIC 9(06) COMP.
               10  SCB-A-003210             PIC 9(09) COMP VALUE 3210.
               10  SCB-B-003211             PIC S9(07)V99 COMP-3 VALUE +0003211.
               10  SCB-C-003212             PIC X(24) VALUE 'FIELD-003212-ALPHA'.
           05  SCB-GRP-0293.
               10  SCB-D-003213             PIC 9(04) VALUE 2491.
               10  SCB-DESC-003213          PIC X(18) VALUE 'DESC-041769'.
               10  SCB-E-003214             OCCURS 4 TIMES.
                   15  SCB-EK-003214        PIC X(10).
                   15  SCB-EV-003214        PIC 9(06) COMP.
               10  SCB-A-003215             PIC 9(09) COMP VALUE 3215.
               10  SCB-B-003216             PIC S9(07)V99 COMP-3 VALUE +0003216.
               10  SCB-C-003217             PIC X(24) VALUE 'FIELD-003217-ALPHA'.
               10  SCB-D-003218             PIC 9(04) VALUE 2526.
               10  SCB-DESC-003218          PIC X(18) VALUE 'DESC-041834'.
               10  SCB-E-003219             OCCURS 5 TIMES.
                   15  SCB-EK-003219        PIC X(10).
                   15  SCB-EV-003219        PIC 9(06) COMP.
               10  SCB-A-003220             PIC 9(09) COMP VALUE 3220.
               10  SCB-B-003221             PIC S9(07)V99 COMP-3 VALUE +0003221.
               10  SCB-C-003222             PIC X(24) VALUE 'FIELD-003222-ALPHA'.
               10  SCB-D-003223             PIC 9(04) VALUE 2561.
               10  SCB-DESC-003223          PIC X(18) VALUE 'DESC-041899'.
               10  SCB-E-003224             OCCURS 2 TIMES.
                   15  SCB-EK-003224        PIC X(10).
                   15  SCB-EV-003224        PIC 9(06) COMP.
               10  SCB-A-003225             PIC 9(09) COMP VALUE 3225.
               10  SCB-B-003226             PIC S9(07)V99 COMP-3 VALUE +0003226.
           05  SCB-GRP-0294.
               10  SCB-C-003227             PIC X(24) VALUE 'FIELD-003227-ALPHA'.
               10  SCB-D-003228             PIC 9(04) VALUE 2596.
               10  SCB-DESC-003228          PIC X(18) VALUE 'DESC-041964'.
               10  SCB-E-003229             OCCURS 3 TIMES.
                   15  SCB-EK-003229        PIC X(10).
                   15  SCB-EV-003229        PIC 9(06) COMP.
               10  SCB-A-003230             PIC 9(09) COMP VALUE 3230.
               10  SCB-B-003231             PIC S9(07)V99 COMP-3 VALUE +0003231.
               10  SCB-C-003232             PIC X(24) VALUE 'FIELD-003232-ALPHA'.
               10  SCB-D-003233             PIC 9(04) VALUE 2631.
               10  SCB-DESC-003233          PIC X(18) VALUE 'DESC-042029'.
               10  SCB-E-003234             OCCURS 4 TIMES.
                   15  SCB-EK-003234        PIC X(10).
                   15  SCB-EV-003234        PIC 9(06) COMP.
           05  SCB-ALT-0049 REDEFINES SCB-GRP-0294.
               10  SCB-ALT-FLAG-0049      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0049   PIC X(64).
           05  SCB-GRP-0295.
               10  SCB-A-003235             PIC 9(09) COMP VALUE 3235.
               10  SCB-B-003236             PIC S9(07)V99 COMP-3 VALUE +0003236.
               10  SCB-C-003237             PIC X(24) VALUE 'FIELD-003237-ALPHA'.
               10  SCB-D-003238             PIC 9(04) VALUE 2666.
               10  SCB-DESC-003238          PIC X(18) VALUE 'DESC-042094'.
               10  SCB-E-003239             OCCURS 5 TIMES.
                   15  SCB-EK-003239        PIC X(10).
                   15  SCB-EV-003239        PIC 9(06) COMP.
               10  SCB-A-003240             PIC 9(09) COMP VALUE 3240.
               10  SCB-B-003241             PIC S9(07)V99 COMP-3 VALUE +0003241.
               10  SCB-C-003242             PIC X(24) VALUE 'FIELD-003242-ALPHA'.
               10  SCB-D-003243             PIC 9(04) VALUE 2701.
               10  SCB-DESC-003243          PIC X(18) VALUE 'DESC-042159'.
           05  SCB-GRP-0296.
               10  SCB-E-003244             OCCURS 2 TIMES.
                   15  SCB-EK-003244        PIC X(10).
                   15  SCB-EV-003244        PIC 9(06) COMP.
               10  SCB-A-003245             PIC 9(09) COMP VALUE 3245.
               10  SCB-B-003246             PIC S9(07)V99 COMP-3 VALUE +0003246.
               10  SCB-C-003247             PIC X(24) VALUE 'FIELD-003247-ALPHA'.
               10  SCB-D-003248             PIC 9(04) VALUE 2736.
               10  SCB-DESC-003248          PIC X(18) VALUE 'DESC-042224'.
               10  SCB-E-003249             OCCURS 3 TIMES.
                   15  SCB-EK-003249        PIC X(10).
                   15  SCB-EV-003249        PIC 9(06) COMP.
               10  SCB-A-003250             PIC 9(09) COMP VALUE 3250.
               10  SCB-B-003251             PIC S9(07)V99 COMP-3 VALUE +0003251.
               10  SCB-C-003252             PIC X(24) VALUE 'FIELD-003252-ALPHA'.
               10  SCB-D-003253             PIC 9(04) VALUE 2771.
               10  SCB-DESC-003253          PIC X(18) VALUE 'DESC-042289'.
           05  SCB-GRP-0297.
               10  SCB-E-003254             OCCURS 4 TIMES.
                   15  SCB-EK-003254        PIC X(10).
                   15  SCB-EV-003254        PIC 9(06) COMP.
               10  SCB-A-003255             PIC 9(09) COMP VALUE 3255.
               10  SCB-B-003256             PIC S9(07)V99 COMP-3 VALUE +0003256.
               10  SCB-C-003257             PIC X(24) VALUE 'FIELD-003257-ALPHA'.
               10  SCB-D-003258             PIC 9(04) VALUE 2806.
               10  SCB-DESC-003258          PIC X(18) VALUE 'DESC-042354'.
               10  SCB-E-003259             OCCURS 5 TIMES.
                   15  SCB-EK-003259        PIC X(10).
                   15  SCB-EV-003259        PIC 9(06) COMP.
               10  SCB-A-003260             PIC 9(09) COMP VALUE 3260.
               10  SCB-B-003261             PIC S9(07)V99 COMP-3 VALUE +0003261.
               10  SCB-C-003262             PIC X(24) VALUE 'FIELD-003262-ALPHA'.
               10  SCB-D-003263             PIC 9(04) VALUE 2841.
               10  SCB-DESC-003263          PIC X(18) VALUE 'DESC-042419'.
               10  SCB-E-003264             OCCURS 2 TIMES.
                   15  SCB-EK-003264        PIC X(10).
                   15  SCB-EV-003264        PIC 9(06) COMP.
           05  SCB-GRP-0298.
               10  SCB-A-003265             PIC 9(09) COMP VALUE 3265.
               10  SCB-B-003266             PIC S9(07)V99 COMP-3 VALUE +0003266.
               10  SCB-C-003267             PIC X(24) VALUE 'FIELD-003267-ALPHA'.
               10  SCB-D-003268             PIC 9(04) VALUE 2876.
               10  SCB-DESC-003268          PIC X(18) VALUE 'DESC-042484'.
               10  SCB-E-003269             OCCURS 3 TIMES.
                   15  SCB-EK-003269        PIC X(10).
                   15  SCB-EV-003269        PIC 9(06) COMP.
               10  SCB-A-003270             PIC 9(09) COMP VALUE 3270.
               10  SCB-B-003271             PIC S9(07)V99 COMP-3 VALUE +0003271.
               10  SCB-C-003272             PIC X(24) VALUE 'FIELD-003272-ALPHA'.
               10  SCB-D-003273             PIC 9(04) VALUE 2911.
               10  SCB-DESC-003273          PIC X(18) VALUE 'DESC-042549'.
               10  SCB-E-003274             OCCURS 4 TIMES.
                   15  SCB-EK-003274        PIC X(10).
                   15  SCB-EV-003274        PIC 9(06) COMP.
               10  SCB-A-003275             PIC 9(09) COMP VALUE 3275.
               10  SCB-B-003276             PIC S9(07)V99 COMP-3 VALUE +0003276.
           05  SCB-GRP-0299.
               10  SCB-C-003277             PIC X(24) VALUE 'FIELD-003277-ALPHA'.
               10  SCB-D-003278             PIC 9(04) VALUE 2946.
               10  SCB-DESC-003278          PIC X(18) VALUE 'DESC-042614'.
               10  SCB-E-003279             OCCURS 5 TIMES.
                   15  SCB-EK-003279        PIC X(10).
                   15  SCB-EV-003279        PIC 9(06) COMP.
               10  SCB-A-003280             PIC 9(09) COMP VALUE 3280.
               10  SCB-B-003281             PIC S9(07)V99 COMP-3 VALUE +0003281.
               10  SCB-C-003282             PIC X(24) VALUE 'FIELD-003282-ALPHA'.
               10  SCB-D-003283             PIC 9(04) VALUE 2981.
               10  SCB-DESC-003283          PIC X(18) VALUE 'DESC-042679'.
               10  SCB-E-003284             OCCURS 2 TIMES.
                   15  SCB-EK-003284        PIC X(10).
                   15  SCB-EV-003284        PIC 9(06) COMP.
               10  SCB-A-003285             PIC 9(09) COMP VALUE 3285.
               10  SCB-B-003286             PIC S9(07)V99 COMP-3 VALUE +0003286.
               10  SCB-C-003287             PIC X(24) VALUE 'FIELD-003287-ALPHA'.
               10  SCB-D-003288             PIC 9(04) VALUE 3016.
               10  SCB-DESC-003288          PIC X(18) VALUE 'DESC-042744'.
               10  SCB-E-003289             OCCURS 3 TIMES.
                   15  SCB-EK-003289        PIC X(10).
                   15  SCB-EV-003289        PIC 9(06) COMP.
           05  SCB-GRP-0300.
               10  SCB-A-003290             PIC 9(09) COMP VALUE 3290.
               10  SCB-B-003291             PIC S9(07)V99 COMP-3 VALUE +0003291.
               10  SCB-C-003292             PIC X(24) VALUE 'FIELD-003292-ALPHA'.
               10  SCB-D-003293             PIC 9(04) VALUE 3051.
               10  SCB-DESC-003293          PIC X(18) VALUE 'DESC-042809'.
               10  SCB-E-003294             OCCURS 4 TIMES.
                   15  SCB-EK-003294        PIC X(10).
                   15  SCB-EV-003294        PIC 9(06) COMP.
               10  SCB-A-003295             PIC 9(09) COMP VALUE 3295.
               10  SCB-B-003296             PIC S9(07)V99 COMP-3 VALUE +0003296.
               10  SCB-C-003297             PIC X(24) VALUE 'FIELD-003297-ALPHA'.
               10  SCB-D-003298             PIC 9(04) VALUE 3086.
               10  SCB-DESC-003298          PIC X(18) VALUE 'DESC-042874'.
               10  SCB-E-003299             OCCURS 5 TIMES.
                   15  SCB-EK-003299        PIC X(10).
                   15  SCB-EV-003299        PIC 9(06) COMP.
               10  SCB-A-003300             PIC 9(09) COMP VALUE 3300.
               10  SCB-B-003301             PIC S9(07)V99 COMP-3 VALUE +0003301.
               10  SCB-C-003302             PIC X(24) VALUE 'FIELD-003302-ALPHA'.
               10  SCB-D-003303             PIC 9(04) VALUE 3121.
               10  SCB-DESC-003303          PIC X(18) VALUE 'DESC-042939'.
           05  SCB-ALT-0050 REDEFINES SCB-GRP-0300.
               10  SCB-ALT-FLAG-0050      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0050   PIC X(64).
           05  SCB-GRP-0301.
               10  SCB-E-003304             OCCURS 2 TIMES.
                   15  SCB-EK-003304        PIC X(10).
                   15  SCB-EV-003304        PIC 9(06) COMP.
               10  SCB-A-003305             PIC 9(09) COMP VALUE 3305.
               10  SCB-B-003306             PIC S9(07)V99 COMP-3 VALUE +0003306.
               10  SCB-C-003307             PIC X(24) VALUE 'FIELD-003307-ALPHA'.
               10  SCB-D-003308             PIC 9(04) VALUE 3156.
               10  SCB-DESC-003308          PIC X(18) VALUE 'DESC-043004'.
               10  SCB-E-003309             OCCURS 3 TIMES.
                   15  SCB-EK-003309        PIC X(10).
                   15  SCB-EV-003309        PIC 9(06) COMP.
               10  SCB-A-003310             PIC 9(09) COMP VALUE 3310.
               10  SCB-B-003311             PIC S9(07)V99 COMP-3 VALUE +0003311.
           05  SCB-GRP-0302.
               10  SCB-C-003312             PIC X(24) VALUE 'FIELD-003312-ALPHA'.
               10  SCB-D-003313             PIC 9(04) VALUE 3191.
               10  SCB-DESC-003313          PIC X(18) VALUE 'DESC-043069'.
               10  SCB-E-003314             OCCURS 4 TIMES.
                   15  SCB-EK-003314        PIC X(10).
                   15  SCB-EV-003314        PIC 9(06) COMP.
               10  SCB-A-003315             PIC 9(09) COMP VALUE 3315.
               10  SCB-B-003316             PIC S9(07)V99 COMP-3 VALUE +0003316.
               10  SCB-C-003317             PIC X(24) VALUE 'FIELD-003317-ALPHA'.
               10  SCB-D-003318             PIC 9(04) VALUE 3226.
               10  SCB-DESC-003318          PIC X(18) VALUE 'DESC-043134'.
               10  SCB-E-003319             OCCURS 5 TIMES.
                   15  SCB-EK-003319        PIC X(10).
                   15  SCB-EV-003319        PIC 9(06) COMP.
               10  SCB-A-003320             PIC 9(09) COMP VALUE 3320.
           05  SCB-GRP-0303.
               10  SCB-B-003321             PIC S9(07)V99 COMP-3 VALUE +0003321.
               10  SCB-C-003322             PIC X(24) VALUE 'FIELD-003322-ALPHA'.
               10  SCB-D-003323             PIC 9(04) VALUE 3261.
               10  SCB-DESC-003323          PIC X(18) VALUE 'DESC-043199'.
               10  SCB-E-003324             OCCURS 2 TIMES.
                   15  SCB-EK-003324        PIC X(10).
                   15  SCB-EV-003324        PIC 9(06) COMP.
               10  SCB-A-003325             PIC 9(09) COMP VALUE 3325.
               10  SCB-B-003326             PIC S9(07)V99 COMP-3 VALUE +0003326.
               10  SCB-C-003327             PIC X(24) VALUE 'FIELD-003327-ALPHA'.
               10  SCB-D-003328             PIC 9(04) VALUE 3296.
               10  SCB-DESC-003328          PIC X(18) VALUE 'DESC-043264'.
               10  SCB-E-003329             OCCURS 3 TIMES.
                   15  SCB-EK-003329        PIC X(10).
                   15  SCB-EV-003329        PIC 9(06) COMP.
               10  SCB-A-003330             PIC 9(09) COMP VALUE 3330.
           05  SCB-GRP-0304.
               10  SCB-B-003331             PIC S9(07)V99 COMP-3 VALUE +0003331.
               10  SCB-C-003332             PIC X(24) VALUE 'FIELD-003332-ALPHA'.
               10  SCB-D-003333             PIC 9(04) VALUE 3331.
               10  SCB-DESC-003333          PIC X(18) VALUE 'DESC-043329'.
               10  SCB-E-003334             OCCURS 4 TIMES.
                   15  SCB-EK-003334        PIC X(10).
                   15  SCB-EV-003334        PIC 9(06) COMP.
               10  SCB-A-003335             PIC 9(09) COMP VALUE 3335.
               10  SCB-B-003336             PIC S9(07)V99 COMP-3 VALUE +0003336.
               10  SCB-C-003337             PIC X(24) VALUE 'FIELD-003337-ALPHA'.
               10  SCB-D-003338             PIC 9(04) VALUE 3366.
               10  SCB-DESC-003338          PIC X(18) VALUE 'DESC-043394'.
               10  SCB-E-003339             OCCURS 5 TIMES.
                   15  SCB-EK-003339        PIC X(10).
                   15  SCB-EV-003339        PIC 9(06) COMP.
               10  SCB-A-003340             PIC 9(09) COMP VALUE 3340.
               10  SCB-B-003341             PIC S9(07)V99 COMP-3 VALUE +0003341.
           05  SCB-GRP-0305.
               10  SCB-C-003342             PIC X(24) VALUE 'FIELD-003342-ALPHA'.
               10  SCB-D-003343             PIC 9(04) VALUE 3401.
               10  SCB-DESC-003343          PIC X(18) VALUE 'DESC-043459'.
               10  SCB-E-003344             OCCURS 2 TIMES.
                   15  SCB-EK-003344        PIC X(10).
                   15  SCB-EV-003344        PIC 9(06) COMP.
               10  SCB-A-003345             PIC 9(09) COMP VALUE 3345.
               10  SCB-B-003346             PIC S9(07)V99 COMP-3 VALUE +0003346.
               10  SCB-C-003347             PIC X(24) VALUE 'FIELD-003347-ALPHA'.
               10  SCB-D-003348             PIC 9(04) VALUE 3436.
               10  SCB-DESC-003348          PIC X(18) VALUE 'DESC-043524'.
               10  SCB-E-003349             OCCURS 3 TIMES.
                   15  SCB-EK-003349        PIC X(10).
                   15  SCB-EV-003349        PIC 9(06) COMP.
               10  SCB-A-003350             PIC 9(09) COMP VALUE 3350.
               10  SCB-B-003351             PIC S9(07)V99 COMP-3 VALUE +0003351.
               10  SCB-C-003352             PIC X(24) VALUE 'FIELD-003352-ALPHA'.
               10  SCB-D-003353             PIC 9(04) VALUE 3471.
               10  SCB-DESC-003353          PIC X(18) VALUE 'DESC-043589'.
           05  SCB-GRP-0306.
               10  SCB-E-003354             OCCURS 4 TIMES.
                   15  SCB-EK-003354        PIC X(10).
                   15  SCB-EV-003354        PIC 9(06) COMP.
               10  SCB-A-003355             PIC 9(09) COMP VALUE 3355.
               10  SCB-B-003356             PIC S9(07)V99 COMP-3 VALUE +0003356.
               10  SCB-C-003357             PIC X(24) VALUE 'FIELD-003357-ALPHA'.
               10  SCB-D-003358             PIC 9(04) VALUE 3506.
               10  SCB-DESC-003358          PIC X(18) VALUE 'DESC-043654'.
               10  SCB-E-003359             OCCURS 5 TIMES.
                   15  SCB-EK-003359        PIC X(10).
                   15  SCB-EV-003359        PIC 9(06) COMP.
               10  SCB-A-003360             PIC 9(09) COMP VALUE 3360.
               10  SCB-B-003361             PIC S9(07)V99 COMP-3 VALUE +0003361.
               10  SCB-C-003362             PIC X(24) VALUE 'FIELD-003362-ALPHA'.
               10  SCB-D-003363             PIC 9(04) VALUE 3541.
               10  SCB-DESC-003363          PIC X(18) VALUE 'DESC-043719'.
               10  SCB-E-003364             OCCURS 2 TIMES.
                   15  SCB-EK-003364        PIC X(10).
                   15  SCB-EV-003364        PIC 9(06) COMP.
               10  SCB-A-003365             PIC 9(09) COMP VALUE 3365.
               10  SCB-B-003366             PIC S9(07)V99 COMP-3 VALUE +0003366.
           05  SCB-ALT-0051 REDEFINES SCB-GRP-0306.
               10  SCB-ALT-FLAG-0051      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0051   PIC X(64).
           05  SCB-GRP-0307.
               10  SCB-C-003367             PIC X(24) VALUE 'FIELD-003367-ALPHA'.
               10  SCB-D-003368             PIC 9(04) VALUE 3576.
               10  SCB-DESC-003368          PIC X(18) VALUE 'DESC-043784'.
               10  SCB-E-003369             OCCURS 3 TIMES.
                   15  SCB-EK-003369        PIC X(10).
                   15  SCB-EV-003369        PIC 9(06) COMP.
               10  SCB-A-003370             PIC 9(09) COMP VALUE 3370.
               10  SCB-B-003371             PIC S9(07)V99 COMP-3 VALUE +0003371.
               10  SCB-C-003372             PIC X(24) VALUE 'FIELD-003372-ALPHA'.
               10  SCB-D-003373             PIC 9(04) VALUE 3611.
               10  SCB-DESC-003373          PIC X(18) VALUE 'DESC-043849'.
               10  SCB-E-003374             OCCURS 4 TIMES.
                   15  SCB-EK-003374        PIC X(10).
                   15  SCB-EV-003374        PIC 9(06) COMP.
               10  SCB-A-003375             PIC 9(09) COMP VALUE 3375.
               10  SCB-B-003376             PIC S9(07)V99 COMP-3 VALUE +0003376.
               10  SCB-C-003377             PIC X(24) VALUE 'FIELD-003377-ALPHA'.
               10  SCB-D-003378             PIC 9(04) VALUE 3646.
               10  SCB-DESC-003378          PIC X(18) VALUE 'DESC-043914'.
               10  SCB-E-003379             OCCURS 5 TIMES.
                   15  SCB-EK-003379        PIC X(10).
                   15  SCB-EV-003379        PIC 9(06) COMP.
               10  SCB-A-003380             PIC 9(09) COMP VALUE 3380.
           05  SCB-GRP-0308.
               10  SCB-B-003381             PIC S9(07)V99 COMP-3 VALUE +0003381.
               10  SCB-C-003382             PIC X(24) VALUE 'FIELD-003382-ALPHA'.
               10  SCB-D-003383             PIC 9(04) VALUE 3681.
               10  SCB-DESC-003383          PIC X(18) VALUE 'DESC-043979'.
               10  SCB-E-003384             OCCURS 2 TIMES.
                   15  SCB-EK-003384        PIC X(10).
                   15  SCB-EV-003384        PIC 9(06) COMP.
               10  SCB-A-003385             PIC 9(09) COMP VALUE 3385.
               10  SCB-B-003386             PIC S9(07)V99 COMP-3 VALUE +0003386.
               10  SCB-C-003387             PIC X(24) VALUE 'FIELD-003387-ALPHA'.
               10  SCB-D-003388             PIC 9(04) VALUE 3716.
               10  SCB-DESC-003388          PIC X(18) VALUE 'DESC-044044'.
           05  SCB-GRP-0309.
               10  SCB-E-003389             OCCURS 3 TIMES.
                   15  SCB-EK-003389        PIC X(10).
                   15  SCB-EV-003389        PIC 9(06) COMP.
               10  SCB-A-003390             PIC 9(09) COMP VALUE 3390.
               10  SCB-B-003391             PIC S9(07)V99 COMP-3 VALUE +0003391.
               10  SCB-C-003392             PIC X(24) VALUE 'FIELD-003392-ALPHA'.
               10  SCB-D-003393             PIC 9(04) VALUE 3751.
               10  SCB-DESC-003393          PIC X(18) VALUE 'DESC-044109'.
               10  SCB-E-003394             OCCURS 4 TIMES.
                   15  SCB-EK-003394        PIC X(10).
                   15  SCB-EV-003394        PIC 9(06) COMP.
               10  SCB-A-003395             PIC 9(09) COMP VALUE 3395.
               10  SCB-B-003396             PIC S9(07)V99 COMP-3 VALUE +0003396.
               10  SCB-C-003397             PIC X(24) VALUE 'FIELD-003397-ALPHA'.
           05  SCB-GRP-0310.
               10  SCB-D-003398             PIC 9(04) VALUE 3786.
               10  SCB-DESC-003398          PIC X(18) VALUE 'DESC-044174'.
               10  SCB-E-003399             OCCURS 5 TIMES.
                   15  SCB-EK-003399        PIC X(10).
                   15  SCB-EV-003399        PIC 9(06) COMP.
               10  SCB-A-003400             PIC 9(09) COMP VALUE 3400.
               10  SCB-B-003401             PIC S9(07)V99 COMP-3 VALUE +0003401.
               10  SCB-C-003402             PIC X(24) VALUE 'FIELD-003402-ALPHA'.
               10  SCB-D-003403             PIC 9(04) VALUE 3821.
               10  SCB-DESC-003403          PIC X(18) VALUE 'DESC-044239'.
               10  SCB-E-003404             OCCURS 2 TIMES.
                   15  SCB-EK-003404        PIC X(10).
                   15  SCB-EV-003404        PIC 9(06) COMP.
               10  SCB-A-003405             PIC 9(09) COMP VALUE 3405.
               10  SCB-B-003406             PIC S9(07)V99 COMP-3 VALUE +0003406.
               10  SCB-C-003407             PIC X(24) VALUE 'FIELD-003407-ALPHA'.
           05  SCB-GRP-0311.
               10  SCB-D-003408             PIC 9(04) VALUE 3856.
               10  SCB-DESC-003408          PIC X(18) VALUE 'DESC-044304'.
               10  SCB-E-003409             OCCURS 3 TIMES.
                   15  SCB-EK-003409        PIC X(10).
                   15  SCB-EV-003409        PIC 9(06) COMP.
               10  SCB-A-003410             PIC 9(09) COMP VALUE 3410.
               10  SCB-B-003411             PIC S9(07)V99 COMP-3 VALUE +0003411.
               10  SCB-C-003412             PIC X(24) VALUE 'FIELD-003412-ALPHA'.
               10  SCB-D-003413             PIC 9(04) VALUE 3891.
               10  SCB-DESC-003413          PIC X(18) VALUE 'DESC-044369'.
               10  SCB-E-003414             OCCURS 4 TIMES.
                   15  SCB-EK-003414        PIC X(10).
                   15  SCB-EV-003414        PIC 9(06) COMP.
               10  SCB-A-003415             PIC 9(09) COMP VALUE 3415.
               10  SCB-B-003416             PIC S9(07)V99 COMP-3 VALUE +0003416.
               10  SCB-C-003417             PIC X(24) VALUE 'FIELD-003417-ALPHA'.
               10  SCB-D-003418             PIC 9(04) VALUE 3926.
               10  SCB-DESC-003418          PIC X(18) VALUE 'DESC-044434'.
           05  SCB-GRP-0312.
               10  SCB-E-003419             OCCURS 5 TIMES.
                   15  SCB-EK-003419        PIC X(10).
                   15  SCB-EV-003419        PIC 9(06) COMP.
               10  SCB-A-003420             PIC 9(09) COMP VALUE 3420.
               10  SCB-B-003421             PIC S9(07)V99 COMP-3 VALUE +0003421.
               10  SCB-C-003422             PIC X(24) VALUE 'FIELD-003422-ALPHA'.
               10  SCB-D-003423             PIC 9(04) VALUE 3961.
               10  SCB-DESC-003423          PIC X(18) VALUE 'DESC-044499'.
               10  SCB-E-003424             OCCURS 2 TIMES.
                   15  SCB-EK-003424        PIC X(10).
                   15  SCB-EV-003424        PIC 9(06) COMP.
               10  SCB-A-003425             PIC 9(09) COMP VALUE 3425.
               10  SCB-B-003426             PIC S9(07)V99 COMP-3 VALUE +0003426.
               10  SCB-C-003427             PIC X(24) VALUE 'FIELD-003427-ALPHA'.
               10  SCB-D-003428             PIC 9(04) VALUE 3996.
               10  SCB-DESC-003428          PIC X(18) VALUE 'DESC-044564'.
               10  SCB-E-003429             OCCURS 3 TIMES.
                   15  SCB-EK-003429        PIC X(10).
                   15  SCB-EV-003429        PIC 9(06) COMP.
               10  SCB-A-003430             PIC 9(09) COMP VALUE 3430.
           05  SCB-ALT-0052 REDEFINES SCB-GRP-0312.
               10  SCB-ALT-FLAG-0052      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0052   PIC X(64).
           05  SCB-GRP-0313.
               10  SCB-B-003431             PIC S9(07)V99 COMP-3 VALUE +0003431.
               10  SCB-C-003432             PIC X(24) VALUE 'FIELD-003432-ALPHA'.
               10  SCB-D-003433             PIC 9(04) VALUE 4031.
               10  SCB-DESC-003433          PIC X(18) VALUE 'DESC-044629'.
               10  SCB-E-003434             OCCURS 4 TIMES.
                   15  SCB-EK-003434        PIC X(10).
                   15  SCB-EV-003434        PIC 9(06) COMP.
               10  SCB-A-003435             PIC 9(09) COMP VALUE 3435.
               10  SCB-B-003436             PIC S9(07)V99 COMP-3 VALUE +0003436.
               10  SCB-C-003437             PIC X(24) VALUE 'FIELD-003437-ALPHA'.
               10  SCB-D-003438             PIC 9(04) VALUE 4066.
               10  SCB-DESC-003438          PIC X(18) VALUE 'DESC-044694'.
               10  SCB-E-003439             OCCURS 5 TIMES.
                   15  SCB-EK-003439        PIC X(10).
                   15  SCB-EV-003439        PIC 9(06) COMP.
               10  SCB-A-003440             PIC 9(09) COMP VALUE 3440.
               10  SCB-B-003441             PIC S9(07)V99 COMP-3 VALUE +0003441.
               10  SCB-C-003442             PIC X(24) VALUE 'FIELD-003442-ALPHA'.
               10  SCB-D-003443             PIC 9(04) VALUE 4101.
               10  SCB-DESC-003443          PIC X(18) VALUE 'DESC-044759'.
           05  SCB-GRP-0314.
               10  SCB-E-003444             OCCURS 2 TIMES.
                   15  SCB-EK-003444        PIC X(10).
                   15  SCB-EV-003444        PIC 9(06) COMP.
               10  SCB-A-003445             PIC 9(09) COMP VALUE 3445.
               10  SCB-B-003446             PIC S9(07)V99 COMP-3 VALUE +0003446.
               10  SCB-C-003447             PIC X(24) VALUE 'FIELD-003447-ALPHA'.
               10  SCB-D-003448             PIC 9(04) VALUE 4136.
               10  SCB-DESC-003448          PIC X(18) VALUE 'DESC-044824'.
               10  SCB-E-003449             OCCURS 3 TIMES.
                   15  SCB-EK-003449        PIC X(10).
                   15  SCB-EV-003449        PIC 9(06) COMP.
               10  SCB-A-003450             PIC 9(09) COMP VALUE 3450.
               10  SCB-B-003451             PIC S9(07)V99 COMP-3 VALUE +0003451.
               10  SCB-C-003452             PIC X(24) VALUE 'FIELD-003452-ALPHA'.
               10  SCB-D-003453             PIC 9(04) VALUE 4171.
               10  SCB-DESC-003453          PIC X(18) VALUE 'DESC-044889'.
               10  SCB-E-003454             OCCURS 4 TIMES.
                   15  SCB-EK-003454        PIC X(10).
                   15  SCB-EV-003454        PIC 9(06) COMP.
               10  SCB-A-003455             PIC 9(09) COMP VALUE 3455.
               10  SCB-B-003456             PIC S9(07)V99 COMP-3 VALUE +0003456.
               10  SCB-C-003457             PIC X(24) VALUE 'FIELD-003457-ALPHA'.
           05  SCB-GRP-0315.
               10  SCB-D-003458             PIC 9(04) VALUE 4206.
               10  SCB-DESC-003458          PIC X(18) VALUE 'DESC-044954'.
               10  SCB-E-003459             OCCURS 5 TIMES.
                   15  SCB-EK-003459        PIC X(10).
                   15  SCB-EV-003459        PIC 9(06) COMP.
               10  SCB-A-003460             PIC 9(09) COMP VALUE 3460.
               10  SCB-B-003461             PIC S9(07)V99 COMP-3 VALUE +0003461.
               10  SCB-C-003462             PIC X(24) VALUE 'FIELD-003462-ALPHA'.
               10  SCB-D-003463             PIC 9(04) VALUE 4241.
               10  SCB-DESC-003463          PIC X(18) VALUE 'DESC-045019'.
               10  SCB-E-003464             OCCURS 2 TIMES.
                   15  SCB-EK-003464        PIC X(10).
                   15  SCB-EV-003464        PIC 9(06) COMP.
               10  SCB-A-003465             PIC 9(09) COMP VALUE 3465.
           05  SCB-GRP-0316.
               10  SCB-B-003466             PIC S9(07)V99 COMP-3 VALUE +0003466.
               10  SCB-C-003467             PIC X(24) VALUE 'FIELD-003467-ALPHA'.
               10  SCB-D-003468             PIC 9(04) VALUE 4276.
               10  SCB-DESC-003468          PIC X(18) VALUE 'DESC-045084'.
               10  SCB-E-003469             OCCURS 3 TIMES.
                   15  SCB-EK-003469        PIC X(10).
                   15  SCB-EV-003469        PIC 9(06) COMP.
               10  SCB-A-003470             PIC 9(09) COMP VALUE 3470.
               10  SCB-B-003471             PIC S9(07)V99 COMP-3 VALUE +0003471.
               10  SCB-C-003472             PIC X(24) VALUE 'FIELD-003472-ALPHA'.
               10  SCB-D-003473             PIC 9(04) VALUE 4311.
               10  SCB-DESC-003473          PIC X(18) VALUE 'DESC-045149'.
               10  SCB-E-003474             OCCURS 4 TIMES.
                   15  SCB-EK-003474        PIC X(10).
                   15  SCB-EV-003474        PIC 9(06) COMP.
           05  SCB-GRP-0317.
               10  SCB-A-003475             PIC 9(09) COMP VALUE 3475.
               10  SCB-B-003476             PIC S9(07)V99 COMP-3 VALUE +0003476.
               10  SCB-C-003477             PIC X(24) VALUE 'FIELD-003477-ALPHA'.
               10  SCB-D-003478             PIC 9(04) VALUE 4346.
               10  SCB-DESC-003478          PIC X(18) VALUE 'DESC-045214'.
               10  SCB-E-003479             OCCURS 5 TIMES.
                   15  SCB-EK-003479        PIC X(10).
                   15  SCB-EV-003479        PIC 9(06) COMP.
               10  SCB-A-003480             PIC 9(09) COMP VALUE 3480.
               10  SCB-B-003481             PIC S9(07)V99 COMP-3 VALUE +0003481.
               10  SCB-C-003482             PIC X(24) VALUE 'FIELD-003482-ALPHA'.
               10  SCB-D-003483             PIC 9(04) VALUE 4381.
               10  SCB-DESC-003483          PIC X(18) VALUE 'DESC-045279'.
               10  SCB-E-003484             OCCURS 2 TIMES.
                   15  SCB-EK-003484        PIC X(10).
                   15  SCB-EV-003484        PIC 9(06) COMP.
           05  SCB-GRP-0318.
               10  SCB-A-003485             PIC 9(09) COMP VALUE 3485.
               10  SCB-B-003486             PIC S9(07)V99 COMP-3 VALUE +0003486.
               10  SCB-C-003487             PIC X(24) VALUE 'FIELD-003487-ALPHA'.
               10  SCB-D-003488             PIC 9(04) VALUE 4416.
               10  SCB-DESC-003488          PIC X(18) VALUE 'DESC-045344'.
               10  SCB-E-003489             OCCURS 3 TIMES.
                   15  SCB-EK-003489        PIC X(10).
                   15  SCB-EV-003489        PIC 9(06) COMP.
               10  SCB-A-003490             PIC 9(09) COMP VALUE 3490.
               10  SCB-B-003491             PIC S9(07)V99 COMP-3 VALUE +0003491.
               10  SCB-C-003492             PIC X(24) VALUE 'FIELD-003492-ALPHA'.
               10  SCB-D-003493             PIC 9(04) VALUE 4451.
               10  SCB-DESC-003493          PIC X(18) VALUE 'DESC-045409'.
               10  SCB-E-003494             OCCURS 4 TIMES.
                   15  SCB-EK-003494        PIC X(10).
                   15  SCB-EV-003494        PIC 9(06) COMP.
               10  SCB-A-003495             PIC 9(09) COMP VALUE 3495.
           05  SCB-ALT-0053 REDEFINES SCB-GRP-0318.
               10  SCB-ALT-FLAG-0053      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0053   PIC X(64).
           05  SCB-GRP-0319.
               10  SCB-B-003496             PIC S9(07)V99 COMP-3 VALUE +0003496.
               10  SCB-C-003497             PIC X(24) VALUE 'FIELD-003497-ALPHA'.
               10  SCB-D-003498             PIC 9(04) VALUE 4486.
               10  SCB-DESC-003498          PIC X(18) VALUE 'DESC-045474'.
               10  SCB-E-003499             OCCURS 5 TIMES.
                   15  SCB-EK-003499        PIC X(10).
                   15  SCB-EV-003499        PIC 9(06) COMP.
               10  SCB-A-003500             PIC 9(09) COMP VALUE 3500.
               10  SCB-B-003501             PIC S9(07)V99 COMP-3 VALUE +0003501.
               10  SCB-C-003502             PIC X(24) VALUE 'FIELD-003502-ALPHA'.
               10  SCB-D-003503             PIC 9(04) VALUE 4521.
               10  SCB-DESC-003503          PIC X(18) VALUE 'DESC-045539'.
               10  SCB-E-003504             OCCURS 2 TIMES.
                   15  SCB-EK-003504        PIC X(10).
                   15  SCB-EV-003504        PIC 9(06) COMP.
               10  SCB-A-003505             PIC 9(09) COMP VALUE 3505.
               10  SCB-B-003506             PIC S9(07)V99 COMP-3 VALUE +0003506.
               10  SCB-C-003507             PIC X(24) VALUE 'FIELD-003507-ALPHA'.
           05  SCB-GRP-0320.
               10  SCB-D-003508             PIC 9(04) VALUE 4556.
               10  SCB-DESC-003508          PIC X(18) VALUE 'DESC-045604'.
               10  SCB-E-003509             OCCURS 3 TIMES.
                   15  SCB-EK-003509        PIC X(10).
                   15  SCB-EV-003509        PIC 9(06) COMP.
               10  SCB-A-003510             PIC 9(09) COMP VALUE 3510.
               10  SCB-B-003511             PIC S9(07)V99 COMP-3 VALUE +0003511.
               10  SCB-C-003512             PIC X(24) VALUE 'FIELD-003512-ALPHA'.
               10  SCB-D-003513             PIC 9(04) VALUE 4591.
               10  SCB-DESC-003513          PIC X(18) VALUE 'DESC-045669'.
               10  SCB-E-003514             OCCURS 4 TIMES.
                   15  SCB-EK-003514        PIC X(10).
                   15  SCB-EV-003514        PIC 9(06) COMP.
               10  SCB-A-003515             PIC 9(09) COMP VALUE 3515.
               10  SCB-B-003516             PIC S9(07)V99 COMP-3 VALUE +0003516.
               10  SCB-C-003517             PIC X(24) VALUE 'FIELD-003517-ALPHA'.
               10  SCB-D-003518             PIC 9(04) VALUE 4626.
               10  SCB-DESC-003518          PIC X(18) VALUE 'DESC-045734'.
               10  SCB-E-003519             OCCURS 5 TIMES.
                   15  SCB-EK-003519        PIC X(10).
                   15  SCB-EV-003519        PIC 9(06) COMP.
               10  SCB-A-003520             PIC 9(09) COMP VALUE 3520.
           05  SCB-GRP-0321.
               10  SCB-B-003521             PIC S9(07)V99 COMP-3 VALUE +0003521.
               10  SCB-C-003522             PIC X(24) VALUE 'FIELD-003522-ALPHA'.
               10  SCB-D-003523             PIC 9(04) VALUE 4661.
               10  SCB-DESC-003523          PIC X(18) VALUE 'DESC-045799'.
               10  SCB-E-003524             OCCURS 2 TIMES.
                   15  SCB-EK-003524        PIC X(10).
                   15  SCB-EV-003524        PIC 9(06) COMP.
               10  SCB-A-003525             PIC 9(09) COMP VALUE 3525.
               10  SCB-B-003526             PIC S9(07)V99 COMP-3 VALUE +0003526.
               10  SCB-C-003527             PIC X(24) VALUE 'FIELD-003527-ALPHA'.
               10  SCB-D-003528             PIC 9(04) VALUE 4696.
               10  SCB-DESC-003528          PIC X(18) VALUE 'DESC-045864'.
               10  SCB-E-003529             OCCURS 3 TIMES.
                   15  SCB-EK-003529        PIC X(10).
                   15  SCB-EV-003529        PIC 9(06) COMP.
               10  SCB-A-003530             PIC 9(09) COMP VALUE 3530.
               10  SCB-B-003531             PIC S9(07)V99 COMP-3 VALUE +0003531.
               10  SCB-C-003532             PIC X(24) VALUE 'FIELD-003532-ALPHA'.
               10  SCB-D-003533             PIC 9(04) VALUE 4731.
               10  SCB-DESC-003533          PIC X(18) VALUE 'DESC-045929'.
               10  SCB-E-003534             OCCURS 4 TIMES.
                   15  SCB-EK-003534        PIC X(10).
                   15  SCB-EV-003534        PIC 9(06) COMP.
           05  SCB-GRP-0322.
               10  SCB-A-003535             PIC 9(09) COMP VALUE 3535.
               10  SCB-B-003536             PIC S9(07)V99 COMP-3 VALUE +0003536.
               10  SCB-C-003537             PIC X(24) VALUE 'FIELD-003537-ALPHA'.
               10  SCB-D-003538             PIC 9(04) VALUE 4766.
               10  SCB-DESC-003538          PIC X(18) VALUE 'DESC-045994'.
               10  SCB-E-003539             OCCURS 5 TIMES.
                   15  SCB-EK-003539        PIC X(10).
                   15  SCB-EV-003539        PIC 9(06) COMP.
               10  SCB-A-003540             PIC 9(09) COMP VALUE 3540.
               10  SCB-B-003541             PIC S9(07)V99 COMP-3 VALUE +0003541.
               10  SCB-C-003542             PIC X(24) VALUE 'FIELD-003542-ALPHA'.
           05  SCB-GRP-0323.
               10  SCB-D-003543             PIC 9(04) VALUE 4801.
               10  SCB-DESC-003543          PIC X(18) VALUE 'DESC-046059'.
               10  SCB-E-003544             OCCURS 2 TIMES.
                   15  SCB-EK-003544        PIC X(10).
                   15  SCB-EV-003544        PIC 9(06) COMP.
               10  SCB-A-003545             PIC 9(09) COMP VALUE 3545.
               10  SCB-B-003546             PIC S9(07)V99 COMP-3 VALUE +0003546.
               10  SCB-C-003547             PIC X(24) VALUE 'FIELD-003547-ALPHA'.
               10  SCB-D-003548             PIC 9(04) VALUE 4836.
               10  SCB-DESC-003548          PIC X(18) VALUE 'DESC-046124'.
               10  SCB-E-003549             OCCURS 3 TIMES.
                   15  SCB-EK-003549        PIC X(10).
                   15  SCB-EV-003549        PIC 9(06) COMP.
               10  SCB-A-003550             PIC 9(09) COMP VALUE 3550.
               10  SCB-B-003551             PIC S9(07)V99 COMP-3 VALUE +0003551.
           05  SCB-GRP-0324.
               10  SCB-C-003552             PIC X(24) VALUE 'FIELD-003552-ALPHA'.
               10  SCB-D-003553             PIC 9(04) VALUE 4871.
               10  SCB-DESC-003553          PIC X(18) VALUE 'DESC-046189'.
               10  SCB-E-003554             OCCURS 4 TIMES.
                   15  SCB-EK-003554        PIC X(10).
                   15  SCB-EV-003554        PIC 9(06) COMP.
               10  SCB-A-003555             PIC 9(09) COMP VALUE 3555.
               10  SCB-B-003556             PIC S9(07)V99 COMP-3 VALUE +0003556.
               10  SCB-C-003557             PIC X(24) VALUE 'FIELD-003557-ALPHA'.
               10  SCB-D-003558             PIC 9(04) VALUE 4906.
               10  SCB-DESC-003558          PIC X(18) VALUE 'DESC-046254'.
               10  SCB-E-003559             OCCURS 5 TIMES.
                   15  SCB-EK-003559        PIC X(10).
                   15  SCB-EV-003559        PIC 9(06) COMP.
               10  SCB-A-003560             PIC 9(09) COMP VALUE 3560.
               10  SCB-B-003561             PIC S9(07)V99 COMP-3 VALUE +0003561.
           05  SCB-ALT-0054 REDEFINES SCB-GRP-0324.
               10  SCB-ALT-FLAG-0054      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0054   PIC X(64).
           05  SCB-GRP-0325.
               10  SCB-C-003562             PIC X(24) VALUE 'FIELD-003562-ALPHA'.
               10  SCB-D-003563             PIC 9(04) VALUE 4941.
               10  SCB-DESC-003563          PIC X(18) VALUE 'DESC-046319'.
               10  SCB-E-003564             OCCURS 2 TIMES.
                   15  SCB-EK-003564        PIC X(10).
                   15  SCB-EV-003564        PIC 9(06) COMP.
               10  SCB-A-003565             PIC 9(09) COMP VALUE 3565.
               10  SCB-B-003566             PIC S9(07)V99 COMP-3 VALUE +0003566.
               10  SCB-C-003567             PIC X(24) VALUE 'FIELD-003567-ALPHA'.
               10  SCB-D-003568             PIC 9(04) VALUE 4976.
               10  SCB-DESC-003568          PIC X(18) VALUE 'DESC-046384'.
               10  SCB-E-003569             OCCURS 3 TIMES.
                   15  SCB-EK-003569        PIC X(10).
                   15  SCB-EV-003569        PIC 9(06) COMP.
               10  SCB-A-003570             PIC 9(09) COMP VALUE 3570.
               10  SCB-B-003571             PIC S9(07)V99 COMP-3 VALUE +0003571.
               10  SCB-C-003572             PIC X(24) VALUE 'FIELD-003572-ALPHA'.
           05  SCB-GRP-0326.
               10  SCB-D-003573             PIC 9(04) VALUE 5011.
               10  SCB-DESC-003573          PIC X(18) VALUE 'DESC-046449'.
               10  SCB-E-003574             OCCURS 4 TIMES.
                   15  SCB-EK-003574        PIC X(10).
                   15  SCB-EV-003574        PIC 9(06) COMP.
               10  SCB-A-003575             PIC 9(09) COMP VALUE 3575.
               10  SCB-B-003576             PIC S9(07)V99 COMP-3 VALUE +0003576.
               10  SCB-C-003577             PIC X(24) VALUE 'FIELD-003577-ALPHA'.
               10  SCB-D-003578             PIC 9(04) VALUE 5046.
               10  SCB-DESC-003578          PIC X(18) VALUE 'DESC-046514'.
               10  SCB-E-003579             OCCURS 5 TIMES.
                   15  SCB-EK-003579        PIC X(10).
                   15  SCB-EV-003579        PIC 9(06) COMP.
               10  SCB-A-003580             PIC 9(09) COMP VALUE 3580.
               10  SCB-B-003581             PIC S9(07)V99 COMP-3 VALUE +0003581.
               10  SCB-C-003582             PIC X(24) VALUE 'FIELD-003582-ALPHA'.
               10  SCB-D-003583             PIC 9(04) VALUE 5081.
               10  SCB-DESC-003583          PIC X(18) VALUE 'DESC-046579'.
               10  SCB-E-003584             OCCURS 2 TIMES.
                   15  SCB-EK-003584        PIC X(10).
                   15  SCB-EV-003584        PIC 9(06) COMP.
           05  SCB-GRP-0327.
               10  SCB-A-003585             PIC 9(09) COMP VALUE 3585.
               10  SCB-B-003586             PIC S9(07)V99 COMP-3 VALUE +0003586.
               10  SCB-C-003587             PIC X(24) VALUE 'FIELD-003587-ALPHA'.
               10  SCB-D-003588             PIC 9(04) VALUE 5116.
               10  SCB-DESC-003588          PIC X(18) VALUE 'DESC-046644'.
               10  SCB-E-003589             OCCURS 3 TIMES.
                   15  SCB-EK-003589        PIC X(10).
                   15  SCB-EV-003589        PIC 9(06) COMP.
               10  SCB-A-003590             PIC 9(09) COMP VALUE 3590.
               10  SCB-B-003591             PIC S9(07)V99 COMP-3 VALUE +0003591.
               10  SCB-C-003592             PIC X(24) VALUE 'FIELD-003592-ALPHA'.
               10  SCB-D-003593             PIC 9(04) VALUE 5151.
               10  SCB-DESC-003593          PIC X(18) VALUE 'DESC-046709'.
               10  SCB-E-003594             OCCURS 4 TIMES.
                   15  SCB-EK-003594        PIC X(10).
                   15  SCB-EV-003594        PIC 9(06) COMP.
               10  SCB-A-003595             PIC 9(09) COMP VALUE 3595.
               10  SCB-B-003596             PIC S9(07)V99 COMP-3 VALUE +0003596.
               10  SCB-C-003597             PIC X(24) VALUE 'FIELD-003597-ALPHA'.
           05  SCB-GRP-0328.
               10  SCB-D-003598             PIC 9(04) VALUE 5186.
               10  SCB-DESC-003598          PIC X(18) VALUE 'DESC-046774'.
               10  SCB-E-003599             OCCURS 5 TIMES.
                   15  SCB-EK-003599        PIC X(10).
                   15  SCB-EV-003599        PIC 9(06) COMP.
               10  SCB-A-003600             PIC 9(09) COMP VALUE 3600.
               10  SCB-B-003601             PIC S9(07)V99 COMP-3 VALUE +0003601.
               10  SCB-C-003602             PIC X(24) VALUE 'FIELD-003602-ALPHA'.
               10  SCB-D-003603             PIC 9(04) VALUE 5221.
               10  SCB-DESC-003603          PIC X(18) VALUE 'DESC-046839'.
               10  SCB-E-003604             OCCURS 2 TIMES.
                   15  SCB-EK-003604        PIC X(10).
                   15  SCB-EV-003604        PIC 9(06) COMP.
               10  SCB-A-003605             PIC 9(09) COMP VALUE 3605.
               10  SCB-B-003606             PIC S9(07)V99 COMP-3 VALUE +0003606.
               10  SCB-C-003607             PIC X(24) VALUE 'FIELD-003607-ALPHA'.
               10  SCB-D-003608             PIC 9(04) VALUE 5256.
               10  SCB-DESC-003608          PIC X(18) VALUE 'DESC-046904'.
               10  SCB-E-003609             OCCURS 3 TIMES.
                   15  SCB-EK-003609        PIC X(10).
                   15  SCB-EV-003609        PIC 9(06) COMP.
               10  SCB-A-003610             PIC 9(09) COMP VALUE 3610.
               10  SCB-B-003611             PIC S9(07)V99 COMP-3 VALUE +0003611.
           05  SCB-GRP-0329.
               10  SCB-C-003612             PIC X(24) VALUE 'FIELD-003612-ALPHA'.
               10  SCB-D-003613             PIC 9(04) VALUE 5291.
               10  SCB-DESC-003613          PIC X(18) VALUE 'DESC-046969'.
               10  SCB-E-003614             OCCURS 4 TIMES.
                   15  SCB-EK-003614        PIC X(10).
                   15  SCB-EV-003614        PIC 9(06) COMP.
               10  SCB-A-003615             PIC 9(09) COMP VALUE 3615.
               10  SCB-B-003616             PIC S9(07)V99 COMP-3 VALUE +0003616.
               10  SCB-C-003617             PIC X(24) VALUE 'FIELD-003617-ALPHA'.
               10  SCB-D-003618             PIC 9(04) VALUE 5326.
               10  SCB-DESC-003618          PIC X(18) VALUE 'DESC-047034'.
               10  SCB-E-003619             OCCURS 5 TIMES.
                   15  SCB-EK-003619        PIC X(10).
                   15  SCB-EV-003619        PIC 9(06) COMP.
           05  SCB-GRP-0330.
               10  SCB-A-003620             PIC 9(09) COMP VALUE 3620.
               10  SCB-B-003621             PIC S9(07)V99 COMP-3 VALUE +0003621.
               10  SCB-C-003622             PIC X(24) VALUE 'FIELD-003622-ALPHA'.
               10  SCB-D-003623             PIC 9(04) VALUE 5361.
               10  SCB-DESC-003623          PIC X(18) VALUE 'DESC-047099'.
               10  SCB-E-003624             OCCURS 2 TIMES.
                   15  SCB-EK-003624        PIC X(10).
                   15  SCB-EV-003624        PIC 9(06) COMP.
               10  SCB-A-003625             PIC 9(09) COMP VALUE 3625.
               10  SCB-B-003626             PIC S9(07)V99 COMP-3 VALUE +0003626.
               10  SCB-C-003627             PIC X(24) VALUE 'FIELD-003627-ALPHA'.
               10  SCB-D-003628             PIC 9(04) VALUE 5396.
               10  SCB-DESC-003628          PIC X(18) VALUE 'DESC-047164'.
           05  SCB-ALT-0055 REDEFINES SCB-GRP-0330.
               10  SCB-ALT-FLAG-0055      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0055   PIC X(64).
           05  SCB-GRP-0331.
               10  SCB-E-003629             OCCURS 3 TIMES.
                   15  SCB-EK-003629        PIC X(10).
                   15  SCB-EV-003629        PIC 9(06) COMP.
               10  SCB-A-003630             PIC 9(09) COMP VALUE 3630.
               10  SCB-B-003631             PIC S9(07)V99 COMP-3 VALUE +0003631.
               10  SCB-C-003632             PIC X(24) VALUE 'FIELD-003632-ALPHA'.
               10  SCB-D-003633             PIC 9(04) VALUE 5431.
               10  SCB-DESC-003633          PIC X(18) VALUE 'DESC-047229'.
               10  SCB-E-003634             OCCURS 4 TIMES.
                   15  SCB-EK-003634        PIC X(10).
                   15  SCB-EV-003634        PIC 9(06) COMP.
               10  SCB-A-003635             PIC 9(09) COMP VALUE 3635.
               10  SCB-B-003636             PIC S9(07)V99 COMP-3 VALUE +0003636.
               10  SCB-C-003637             PIC X(24) VALUE 'FIELD-003637-ALPHA'.
               10  SCB-D-003638             PIC 9(04) VALUE 5466.
               10  SCB-DESC-003638          PIC X(18) VALUE 'DESC-047294'.
           05  SCB-GRP-0332.
               10  SCB-E-003639             OCCURS 5 TIMES.
                   15  SCB-EK-003639        PIC X(10).
                   15  SCB-EV-003639        PIC 9(06) COMP.
               10  SCB-A-003640             PIC 9(09) COMP VALUE 3640.
               10  SCB-B-003641             PIC S9(07)V99 COMP-3 VALUE +0003641.
               10  SCB-C-003642             PIC X(24) VALUE 'FIELD-003642-ALPHA'.
               10  SCB-D-003643             PIC 9(04) VALUE 5501.
               10  SCB-DESC-003643          PIC X(18) VALUE 'DESC-047359'.
               10  SCB-E-003644             OCCURS 2 TIMES.
                   15  SCB-EK-003644        PIC X(10).
                   15  SCB-EV-003644        PIC 9(06) COMP.
               10  SCB-A-003645             PIC 9(09) COMP VALUE 3645.
               10  SCB-B-003646             PIC S9(07)V99 COMP-3 VALUE +0003646.
               10  SCB-C-003647             PIC X(24) VALUE 'FIELD-003647-ALPHA'.
               10  SCB-D-003648             PIC 9(04) VALUE 5536.
               10  SCB-DESC-003648          PIC X(18) VALUE 'DESC-047424'.
               10  SCB-E-003649             OCCURS 3 TIMES.
                   15  SCB-EK-003649        PIC X(10).
                   15  SCB-EV-003649        PIC 9(06) COMP.
           05  SCB-GRP-0333.
               10  SCB-A-003650             PIC 9(09) COMP VALUE 3650.
               10  SCB-B-003651             PIC S9(07)V99 COMP-3 VALUE +0003651.
               10  SCB-C-003652             PIC X(24) VALUE 'FIELD-003652-ALPHA'.
               10  SCB-D-003653             PIC 9(04) VALUE 5571.
               10  SCB-DESC-003653          PIC X(18) VALUE 'DESC-047489'.
               10  SCB-E-003654             OCCURS 4 TIMES.
                   15  SCB-EK-003654        PIC X(10).
                   15  SCB-EV-003654        PIC 9(06) COMP.
               10  SCB-A-003655             PIC 9(09) COMP VALUE 3655.
               10  SCB-B-003656             PIC S9(07)V99 COMP-3 VALUE +0003656.
               10  SCB-C-003657             PIC X(24) VALUE 'FIELD-003657-ALPHA'.
               10  SCB-D-003658             PIC 9(04) VALUE 5606.
               10  SCB-DESC-003658          PIC X(18) VALUE 'DESC-047554'.
               10  SCB-E-003659             OCCURS 5 TIMES.
                   15  SCB-EK-003659        PIC X(10).
                   15  SCB-EV-003659        PIC 9(06) COMP.
               10  SCB-A-003660             PIC 9(09) COMP VALUE 3660.
               10  SCB-B-003661             PIC S9(07)V99 COMP-3 VALUE +0003661.
           05  SCB-GRP-0334.
               10  SCB-C-003662             PIC X(24) VALUE 'FIELD-003662-ALPHA'.
               10  SCB-D-003663             PIC 9(04) VALUE 5641.
               10  SCB-DESC-003663          PIC X(18) VALUE 'DESC-047619'.
               10  SCB-E-003664             OCCURS 2 TIMES.
                   15  SCB-EK-003664        PIC X(10).
                   15  SCB-EV-003664        PIC 9(06) COMP.
               10  SCB-A-003665             PIC 9(09) COMP VALUE 3665.
               10  SCB-B-003666             PIC S9(07)V99 COMP-3 VALUE +0003666.
               10  SCB-C-003667             PIC X(24) VALUE 'FIELD-003667-ALPHA'.
               10  SCB-D-003668             PIC 9(04) VALUE 5676.
               10  SCB-DESC-003668          PIC X(18) VALUE 'DESC-047684'.
               10  SCB-E-003669             OCCURS 3 TIMES.
                   15  SCB-EK-003669        PIC X(10).
                   15  SCB-EV-003669        PIC 9(06) COMP.
               10  SCB-A-003670             PIC 9(09) COMP VALUE 3670.
               10  SCB-B-003671             PIC S9(07)V99 COMP-3 VALUE +0003671.
               10  SCB-C-003672             PIC X(24) VALUE 'FIELD-003672-ALPHA'.
               10  SCB-D-003673             PIC 9(04) VALUE 5711.
               10  SCB-DESC-003673          PIC X(18) VALUE 'DESC-047749'.
               10  SCB-E-003674             OCCURS 4 TIMES.
                   15  SCB-EK-003674        PIC X(10).
                   15  SCB-EV-003674        PIC 9(06) COMP.
           05  SCB-GRP-0335.
               10  SCB-A-003675             PIC 9(09) COMP VALUE 3675.
               10  SCB-B-003676             PIC S9(07)V99 COMP-3 VALUE +0003676.
               10  SCB-C-003677             PIC X(24) VALUE 'FIELD-003677-ALPHA'.
               10  SCB-D-003678             PIC 9(04) VALUE 5746.
               10  SCB-DESC-003678          PIC X(18) VALUE 'DESC-047814'.
               10  SCB-E-003679             OCCURS 5 TIMES.
                   15  SCB-EK-003679        PIC X(10).
                   15  SCB-EV-003679        PIC 9(06) COMP.
               10  SCB-A-003680             PIC 9(09) COMP VALUE 3680.
               10  SCB-B-003681             PIC S9(07)V99 COMP-3 VALUE +0003681.
               10  SCB-C-003682             PIC X(24) VALUE 'FIELD-003682-ALPHA'.
               10  SCB-D-003683             PIC 9(04) VALUE 5781.
               10  SCB-DESC-003683          PIC X(18) VALUE 'DESC-047879'.
               10  SCB-E-003684             OCCURS 2 TIMES.
                   15  SCB-EK-003684        PIC X(10).
                   15  SCB-EV-003684        PIC 9(06) COMP.
               10  SCB-A-003685             PIC 9(09) COMP VALUE 3685.
               10  SCB-B-003686             PIC S9(07)V99 COMP-3 VALUE +0003686.
               10  SCB-C-003687             PIC X(24) VALUE 'FIELD-003687-ALPHA'.
               10  SCB-D-003688             PIC 9(04) VALUE 5816.
               10  SCB-DESC-003688          PIC X(18) VALUE 'DESC-047944'.
           05  SCB-GRP-0336.
               10  SCB-E-003689             OCCURS 3 TIMES.
                   15  SCB-EK-003689        PIC X(10).
                   15  SCB-EV-003689        PIC 9(06) COMP.
               10  SCB-A-003690             PIC 9(09) COMP VALUE 3690.
               10  SCB-B-003691             PIC S9(07)V99 COMP-3 VALUE +0003691.
               10  SCB-C-003692             PIC X(24) VALUE 'FIELD-003692-ALPHA'.
               10  SCB-D-003693             PIC 9(04) VALUE 5851.
               10  SCB-DESC-003693          PIC X(18) VALUE 'DESC-048009'.
               10  SCB-E-003694             OCCURS 4 TIMES.
                   15  SCB-EK-003694        PIC X(10).
                   15  SCB-EV-003694        PIC 9(06) COMP.
               10  SCB-A-003695             PIC 9(09) COMP VALUE 3695.
               10  SCB-B-003696             PIC S9(07)V99 COMP-3 VALUE +0003696.
           05  SCB-ALT-0056 REDEFINES SCB-GRP-0336.
               10  SCB-ALT-FLAG-0056      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0056   PIC X(64).
           05  SCB-GRP-0337.
               10  SCB-C-003697             PIC X(24) VALUE 'FIELD-003697-ALPHA'.
               10  SCB-D-003698             PIC 9(04) VALUE 5886.
               10  SCB-DESC-003698          PIC X(18) VALUE 'DESC-048074'.
               10  SCB-E-003699             OCCURS 5 TIMES.
                   15  SCB-EK-003699        PIC X(10).
                   15  SCB-EV-003699        PIC 9(06) COMP.
               10  SCB-A-003700             PIC 9(09) COMP VALUE 3700.
               10  SCB-B-003701             PIC S9(07)V99 COMP-3 VALUE +0003701.
               10  SCB-C-003702             PIC X(24) VALUE 'FIELD-003702-ALPHA'.
               10  SCB-D-003703             PIC 9(04) VALUE 5921.
               10  SCB-DESC-003703          PIC X(18) VALUE 'DESC-048139'.
               10  SCB-E-003704             OCCURS 2 TIMES.
                   15  SCB-EK-003704        PIC X(10).
                   15  SCB-EV-003704        PIC 9(06) COMP.
               10  SCB-A-003705             PIC 9(09) COMP VALUE 3705.
           05  SCB-GRP-0338.
               10  SCB-B-003706             PIC S9(07)V99 COMP-3 VALUE +0003706.
               10  SCB-C-003707             PIC X(24) VALUE 'FIELD-003707-ALPHA'.
               10  SCB-D-003708             PIC 9(04) VALUE 5956.
               10  SCB-DESC-003708          PIC X(18) VALUE 'DESC-048204'.
               10  SCB-E-003709             OCCURS 3 TIMES.
                   15  SCB-EK-003709        PIC X(10).
                   15  SCB-EV-003709        PIC 9(06) COMP.
               10  SCB-A-003710             PIC 9(09) COMP VALUE 3710.
               10  SCB-B-003711             PIC S9(07)V99 COMP-3 VALUE +0003711.
               10  SCB-C-003712             PIC X(24) VALUE 'FIELD-003712-ALPHA'.
               10  SCB-D-003713             PIC 9(04) VALUE 5991.
               10  SCB-DESC-003713          PIC X(18) VALUE 'DESC-048269'.
               10  SCB-E-003714             OCCURS 4 TIMES.
                   15  SCB-EK-003714        PIC X(10).
                   15  SCB-EV-003714        PIC 9(06) COMP.
               10  SCB-A-003715             PIC 9(09) COMP VALUE 3715.
           05  SCB-GRP-0339.
               10  SCB-B-003716             PIC S9(07)V99 COMP-3 VALUE +0003716.
               10  SCB-C-003717             PIC X(24) VALUE 'FIELD-003717-ALPHA'.
               10  SCB-D-003718             PIC 9(04) VALUE 6026.
               10  SCB-DESC-003718          PIC X(18) VALUE 'DESC-048334'.
               10  SCB-E-003719             OCCURS 5 TIMES.
                   15  SCB-EK-003719        PIC X(10).
                   15  SCB-EV-003719        PIC 9(06) COMP.
               10  SCB-A-003720             PIC 9(09) COMP VALUE 3720.
               10  SCB-B-003721             PIC S9(07)V99 COMP-3 VALUE +0003721.
               10  SCB-C-003722             PIC X(24) VALUE 'FIELD-003722-ALPHA'.
               10  SCB-D-003723             PIC 9(04) VALUE 6061.
               10  SCB-DESC-003723          PIC X(18) VALUE 'DESC-048399'.
               10  SCB-E-003724             OCCURS 2 TIMES.
                   15  SCB-EK-003724        PIC X(10).
                   15  SCB-EV-003724        PIC 9(06) COMP.
               10  SCB-A-003725             PIC 9(09) COMP VALUE 3725.
               10  SCB-B-003726             PIC S9(07)V99 COMP-3 VALUE +0003726.
           05  SCB-GRP-0340.
               10  SCB-C-003727             PIC X(24) VALUE 'FIELD-003727-ALPHA'.
               10  SCB-D-003728             PIC 9(04) VALUE 6096.
               10  SCB-DESC-003728          PIC X(18) VALUE 'DESC-048464'.
               10  SCB-E-003729             OCCURS 3 TIMES.
                   15  SCB-EK-003729        PIC X(10).
                   15  SCB-EV-003729        PIC 9(06) COMP.
               10  SCB-A-003730             PIC 9(09) COMP VALUE 3730.
               10  SCB-B-003731             PIC S9(07)V99 COMP-3 VALUE +0003731.
               10  SCB-C-003732             PIC X(24) VALUE 'FIELD-003732-ALPHA'.
               10  SCB-D-003733             PIC 9(04) VALUE 6131.
               10  SCB-DESC-003733          PIC X(18) VALUE 'DESC-048529'.
               10  SCB-E-003734             OCCURS 4 TIMES.
                   15  SCB-EK-003734        PIC X(10).
                   15  SCB-EV-003734        PIC 9(06) COMP.
               10  SCB-A-003735             PIC 9(09) COMP VALUE 3735.
               10  SCB-B-003736             PIC S9(07)V99 COMP-3 VALUE +0003736.
               10  SCB-C-003737             PIC X(24) VALUE 'FIELD-003737-ALPHA'.
               10  SCB-D-003738             PIC 9(04) VALUE 6166.
               10  SCB-DESC-003738          PIC X(18) VALUE 'DESC-048594'.
           05  SCB-GRP-0341.
               10  SCB-E-003739             OCCURS 5 TIMES.
                   15  SCB-EK-003739        PIC X(10).
                   15  SCB-EV-003739        PIC 9(06) COMP.
               10  SCB-A-003740             PIC 9(09) COMP VALUE 3740.
               10  SCB-B-003741             PIC S9(07)V99 COMP-3 VALUE +0003741.
               10  SCB-C-003742             PIC X(24) VALUE 'FIELD-003742-ALPHA'.
               10  SCB-D-003743             PIC 9(04) VALUE 6201.
               10  SCB-DESC-003743          PIC X(18) VALUE 'DESC-048659'.
               10  SCB-E-003744             OCCURS 2 TIMES.
                   15  SCB-EK-003744        PIC X(10).
                   15  SCB-EV-003744        PIC 9(06) COMP.
               10  SCB-A-003745             PIC 9(09) COMP VALUE 3745.
               10  SCB-B-003746             PIC S9(07)V99 COMP-3 VALUE +0003746.
               10  SCB-C-003747             PIC X(24) VALUE 'FIELD-003747-ALPHA'.
               10  SCB-D-003748             PIC 9(04) VALUE 6236.
               10  SCB-DESC-003748          PIC X(18) VALUE 'DESC-048724'.
               10  SCB-E-003749             OCCURS 3 TIMES.
                   15  SCB-EK-003749        PIC X(10).
                   15  SCB-EV-003749        PIC 9(06) COMP.
               10  SCB-A-003750             PIC 9(09) COMP VALUE 3750.
               10  SCB-B-003751             PIC S9(07)V99 COMP-3 VALUE +0003751.
           05  SCB-GRP-0342.
               10  SCB-C-003752             PIC X(24) VALUE 'FIELD-003752-ALPHA'.
               10  SCB-D-003753             PIC 9(04) VALUE 6271.
               10  SCB-DESC-003753          PIC X(18) VALUE 'DESC-048789'.
               10  SCB-E-003754             OCCURS 4 TIMES.
                   15  SCB-EK-003754        PIC X(10).
                   15  SCB-EV-003754        PIC 9(06) COMP.
               10  SCB-A-003755             PIC 9(09) COMP VALUE 3755.
               10  SCB-B-003756             PIC S9(07)V99 COMP-3 VALUE +0003756.
               10  SCB-C-003757             PIC X(24) VALUE 'FIELD-003757-ALPHA'.
               10  SCB-D-003758             PIC 9(04) VALUE 6306.
               10  SCB-DESC-003758          PIC X(18) VALUE 'DESC-048854'.
               10  SCB-E-003759             OCCURS 5 TIMES.
                   15  SCB-EK-003759        PIC X(10).
                   15  SCB-EV-003759        PIC 9(06) COMP.
               10  SCB-A-003760             PIC 9(09) COMP VALUE 3760.
               10  SCB-B-003761             PIC S9(07)V99 COMP-3 VALUE +0003761.
               10  SCB-C-003762             PIC X(24) VALUE 'FIELD-003762-ALPHA'.
               10  SCB-D-003763             PIC 9(04) VALUE 6341.
               10  SCB-DESC-003763          PIC X(18) VALUE 'DESC-048919'.
               10  SCB-E-003764             OCCURS 2 TIMES.
                   15  SCB-EK-003764        PIC X(10).
                   15  SCB-EV-003764        PIC 9(06) COMP.
               10  SCB-A-003765             PIC 9(09) COMP VALUE 3765.
           05  SCB-ALT-0057 REDEFINES SCB-GRP-0342.
               10  SCB-ALT-FLAG-0057      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0057   PIC X(64).
           05  SCB-GRP-0343.
               10  SCB-B-003766             PIC S9(07)V99 COMP-3 VALUE +0003766.
               10  SCB-C-003767             PIC X(24) VALUE 'FIELD-003767-ALPHA'.
               10  SCB-D-003768             PIC 9(04) VALUE 6376.
               10  SCB-DESC-003768          PIC X(18) VALUE 'DESC-048984'.
               10  SCB-E-003769             OCCURS 3 TIMES.
                   15  SCB-EK-003769        PIC X(10).
                   15  SCB-EV-003769        PIC 9(06) COMP.
               10  SCB-A-003770             PIC 9(09) COMP VALUE 3770.
               10  SCB-B-003771             PIC S9(07)V99 COMP-3 VALUE +0003771.
               10  SCB-C-003772             PIC X(24) VALUE 'FIELD-003772-ALPHA'.
               10  SCB-D-003773             PIC 9(04) VALUE 6411.
               10  SCB-DESC-003773          PIC X(18) VALUE 'DESC-049049'.
           05  SCB-GRP-0344.
               10  SCB-E-003774             OCCURS 4 TIMES.
                   15  SCB-EK-003774        PIC X(10).
                   15  SCB-EV-003774        PIC 9(06) COMP.
               10  SCB-A-003775             PIC 9(09) COMP VALUE 3775.
               10  SCB-B-003776             PIC S9(07)V99 COMP-3 VALUE +0003776.
               10  SCB-C-003777             PIC X(24) VALUE 'FIELD-003777-ALPHA'.
               10  SCB-D-003778             PIC 9(04) VALUE 6446.
               10  SCB-DESC-003778          PIC X(18) VALUE 'DESC-049114'.
               10  SCB-E-003779             OCCURS 5 TIMES.
                   15  SCB-EK-003779        PIC X(10).
                   15  SCB-EV-003779        PIC 9(06) COMP.
               10  SCB-A-003780             PIC 9(09) COMP VALUE 3780.
               10  SCB-B-003781             PIC S9(07)V99 COMP-3 VALUE +0003781.
               10  SCB-C-003782             PIC X(24) VALUE 'FIELD-003782-ALPHA'.
           05  SCB-GRP-0345.
               10  SCB-D-003783             PIC 9(04) VALUE 6481.
               10  SCB-DESC-003783          PIC X(18) VALUE 'DESC-049179'.
               10  SCB-E-003784             OCCURS 2 TIMES.
                   15  SCB-EK-003784        PIC X(10).
                   15  SCB-EV-003784        PIC 9(06) COMP.
               10  SCB-A-003785             PIC 9(09) COMP VALUE 3785.
               10  SCB-B-003786             PIC S9(07)V99 COMP-3 VALUE +0003786.
               10  SCB-C-003787             PIC X(24) VALUE 'FIELD-003787-ALPHA'.
               10  SCB-D-003788             PIC 9(04) VALUE 6516.
               10  SCB-DESC-003788          PIC X(18) VALUE 'DESC-049244'.
               10  SCB-E-003789             OCCURS 3 TIMES.
                   15  SCB-EK-003789        PIC X(10).
                   15  SCB-EV-003789        PIC 9(06) COMP.
               10  SCB-A-003790             PIC 9(09) COMP VALUE 3790.
               10  SCB-B-003791             PIC S9(07)V99 COMP-3 VALUE +0003791.
               10  SCB-C-003792             PIC X(24) VALUE 'FIELD-003792-ALPHA'.
           05  SCB-GRP-0346.
               10  SCB-D-003793             PIC 9(04) VALUE 6551.
               10  SCB-DESC-003793          PIC X(18) VALUE 'DESC-049309'.
               10  SCB-E-003794             OCCURS 4 TIMES.
                   15  SCB-EK-003794        PIC X(10).
                   15  SCB-EV-003794        PIC 9(06) COMP.
               10  SCB-A-003795             PIC 9(09) COMP VALUE 3795.
               10  SCB-B-003796             PIC S9(07)V99 COMP-3 VALUE +0003796.
               10  SCB-C-003797             PIC X(24) VALUE 'FIELD-003797-ALPHA'.
               10  SCB-D-003798             PIC 9(04) VALUE 6586.
               10  SCB-DESC-003798          PIC X(18) VALUE 'DESC-049374'.
               10  SCB-E-003799             OCCURS 5 TIMES.
                   15  SCB-EK-003799        PIC X(10).
                   15  SCB-EV-003799        PIC 9(06) COMP.
               10  SCB-A-003800             PIC 9(09) COMP VALUE 3800.
               10  SCB-B-003801             PIC S9(07)V99 COMP-3 VALUE +0003801.
               10  SCB-C-003802             PIC X(24) VALUE 'FIELD-003802-ALPHA'.
               10  SCB-D-003803             PIC 9(04) VALUE 6621.
               10  SCB-DESC-003803          PIC X(18) VALUE 'DESC-049439'.
           05  SCB-GRP-0347.
               10  SCB-E-003804             OCCURS 2 TIMES.
                   15  SCB-EK-003804        PIC X(10).
                   15  SCB-EV-003804        PIC 9(06) COMP.
               10  SCB-A-003805             PIC 9(09) COMP VALUE 3805.
               10  SCB-B-003806             PIC S9(07)V99 COMP-3 VALUE +0003806.
               10  SCB-C-003807             PIC X(24) VALUE 'FIELD-003807-ALPHA'.
               10  SCB-D-003808             PIC 9(04) VALUE 6656.
               10  SCB-DESC-003808          PIC X(18) VALUE 'DESC-049504'.
               10  SCB-E-003809             OCCURS 3 TIMES.
                   15  SCB-EK-003809        PIC X(10).
                   15  SCB-EV-003809        PIC 9(06) COMP.
               10  SCB-A-003810             PIC 9(09) COMP VALUE 3810.
               10  SCB-B-003811             PIC S9(07)V99 COMP-3 VALUE +0003811.
               10  SCB-C-003812             PIC X(24) VALUE 'FIELD-003812-ALPHA'.
               10  SCB-D-003813             PIC 9(04) VALUE 6691.
               10  SCB-DESC-003813          PIC X(18) VALUE 'DESC-049569'.
               10  SCB-E-003814             OCCURS 4 TIMES.
                   15  SCB-EK-003814        PIC X(10).
                   15  SCB-EV-003814        PIC 9(06) COMP.
               10  SCB-A-003815             PIC 9(09) COMP VALUE 3815.
           05  SCB-GRP-0348.
               10  SCB-B-003816             PIC S9(07)V99 COMP-3 VALUE +0003816.
               10  SCB-C-003817             PIC X(24) VALUE 'FIELD-003817-ALPHA'.
               10  SCB-D-003818             PIC 9(04) VALUE 6726.
               10  SCB-DESC-003818          PIC X(18) VALUE 'DESC-049634'.
               10  SCB-E-003819             OCCURS 5 TIMES.
                   15  SCB-EK-003819        PIC X(10).
                   15  SCB-EV-003819        PIC 9(06) COMP.
               10  SCB-A-003820             PIC 9(09) COMP VALUE 3820.
               10  SCB-B-003821             PIC S9(07)V99 COMP-3 VALUE +0003821.
               10  SCB-C-003822             PIC X(24) VALUE 'FIELD-003822-ALPHA'.
               10  SCB-D-003823             PIC 9(04) VALUE 6761.
               10  SCB-DESC-003823          PIC X(18) VALUE 'DESC-049699'.
               10  SCB-E-003824             OCCURS 2 TIMES.
                   15  SCB-EK-003824        PIC X(10).
                   15  SCB-EV-003824        PIC 9(06) COMP.
               10  SCB-A-003825             PIC 9(09) COMP VALUE 3825.
               10  SCB-B-003826             PIC S9(07)V99 COMP-3 VALUE +0003826.
               10  SCB-C-003827             PIC X(24) VALUE 'FIELD-003827-ALPHA'.
               10  SCB-D-003828             PIC 9(04) VALUE 6796.
               10  SCB-DESC-003828          PIC X(18) VALUE 'DESC-049764'.
           05  SCB-ALT-0058 REDEFINES SCB-GRP-0348.
               10  SCB-ALT-FLAG-0058      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0058   PIC X(64).
           05  SCB-GRP-0349.
               10  SCB-E-003829             OCCURS 3 TIMES.
                   15  SCB-EK-003829        PIC X(10).
                   15  SCB-EV-003829        PIC 9(06) COMP.
               10  SCB-A-003830             PIC 9(09) COMP VALUE 3830.
               10  SCB-B-003831             PIC S9(07)V99 COMP-3 VALUE +0003831.
               10  SCB-C-003832             PIC X(24) VALUE 'FIELD-003832-ALPHA'.
               10  SCB-D-003833             PIC 9(04) VALUE 6831.
               10  SCB-DESC-003833          PIC X(18) VALUE 'DESC-049829'.
               10  SCB-E-003834             OCCURS 4 TIMES.
                   15  SCB-EK-003834        PIC X(10).
                   15  SCB-EV-003834        PIC 9(06) COMP.
               10  SCB-A-003835             PIC 9(09) COMP VALUE 3835.
               10  SCB-B-003836             PIC S9(07)V99 COMP-3 VALUE +0003836.
               10  SCB-C-003837             PIC X(24) VALUE 'FIELD-003837-ALPHA'.
               10  SCB-D-003838             PIC 9(04) VALUE 6866.
               10  SCB-DESC-003838          PIC X(18) VALUE 'DESC-049894'.
               10  SCB-E-003839             OCCURS 5 TIMES.
                   15  SCB-EK-003839        PIC X(10).
                   15  SCB-EV-003839        PIC 9(06) COMP.
               10  SCB-A-003840             PIC 9(09) COMP VALUE 3840.
               10  SCB-B-003841             PIC S9(07)V99 COMP-3 VALUE +0003841.
               10  SCB-C-003842             PIC X(24) VALUE 'FIELD-003842-ALPHA'.
           05  SCB-GRP-0350.
               10  SCB-D-003843             PIC 9(04) VALUE 6901.
               10  SCB-DESC-003843          PIC X(18) VALUE 'DESC-049959'.
               10  SCB-E-003844             OCCURS 2 TIMES.
                   15  SCB-EK-003844        PIC X(10).
                   15  SCB-EV-003844        PIC 9(06) COMP.
               10  SCB-A-003845             PIC 9(09) COMP VALUE 3845.
               10  SCB-B-003846             PIC S9(07)V99 COMP-3 VALUE +0003846.
               10  SCB-C-003847             PIC X(24) VALUE 'FIELD-003847-ALPHA'.
               10  SCB-D-003848             PIC 9(04) VALUE 6936.
               10  SCB-DESC-003848          PIC X(18) VALUE 'DESC-050024'.
               10  SCB-E-003849             OCCURS 3 TIMES.
                   15  SCB-EK-003849        PIC X(10).
                   15  SCB-EV-003849        PIC 9(06) COMP.
               10  SCB-A-003850             PIC 9(09) COMP VALUE 3850.
           05  SCB-GRP-0351.
               10  SCB-B-003851             PIC S9(07)V99 COMP-3 VALUE +0003851.
               10  SCB-C-003852             PIC X(24) VALUE 'FIELD-003852-ALPHA'.
               10  SCB-D-003853             PIC 9(04) VALUE 6971.
               10  SCB-DESC-003853          PIC X(18) VALUE 'DESC-050089'.
               10  SCB-E-003854             OCCURS 4 TIMES.
                   15  SCB-EK-003854        PIC X(10).
                   15  SCB-EV-003854        PIC 9(06) COMP.
               10  SCB-A-003855             PIC 9(09) COMP VALUE 3855.
               10  SCB-B-003856             PIC S9(07)V99 COMP-3 VALUE +0003856.
               10  SCB-C-003857             PIC X(24) VALUE 'FIELD-003857-ALPHA'.
               10  SCB-D-003858             PIC 9(04) VALUE 7006.
               10  SCB-DESC-003858          PIC X(18) VALUE 'DESC-050154'.
               10  SCB-E-003859             OCCURS 5 TIMES.
                   15  SCB-EK-003859        PIC X(10).
                   15  SCB-EV-003859        PIC 9(06) COMP.
           05  SCB-GRP-0352.
               10  SCB-A-003860             PIC 9(09) COMP VALUE 3860.
               10  SCB-B-003861             PIC S9(07)V99 COMP-3 VALUE +0003861.
               10  SCB-C-003862             PIC X(24) VALUE 'FIELD-003862-ALPHA'.
               10  SCB-D-003863             PIC 9(04) VALUE 7041.
               10  SCB-DESC-003863          PIC X(18) VALUE 'DESC-050219'.
               10  SCB-E-003864             OCCURS 2 TIMES.
                   15  SCB-EK-003864        PIC X(10).
                   15  SCB-EV-003864        PIC 9(06) COMP.
               10  SCB-A-003865             PIC 9(09) COMP VALUE 3865.
               10  SCB-B-003866             PIC S9(07)V99 COMP-3 VALUE +0003866.
               10  SCB-C-003867             PIC X(24) VALUE 'FIELD-003867-ALPHA'.
               10  SCB-D-003868             PIC 9(04) VALUE 7076.
               10  SCB-DESC-003868          PIC X(18) VALUE 'DESC-050284'.
               10  SCB-E-003869             OCCURS 3 TIMES.
                   15  SCB-EK-003869        PIC X(10).
                   15  SCB-EV-003869        PIC 9(06) COMP.
           05  SCB-GRP-0353.
               10  SCB-A-003870             PIC 9(09) COMP VALUE 3870.
               10  SCB-B-003871             PIC S9(07)V99 COMP-3 VALUE +0003871.
               10  SCB-C-003872             PIC X(24) VALUE 'FIELD-003872-ALPHA'.
               10  SCB-D-003873             PIC 9(04) VALUE 7111.
               10  SCB-DESC-003873          PIC X(18) VALUE 'DESC-050349'.
               10  SCB-E-003874             OCCURS 4 TIMES.
                   15  SCB-EK-003874        PIC X(10).
                   15  SCB-EV-003874        PIC 9(06) COMP.
               10  SCB-A-003875             PIC 9(09) COMP VALUE 3875.
               10  SCB-B-003876             PIC S9(07)V99 COMP-3 VALUE +0003876.
               10  SCB-C-003877             PIC X(24) VALUE 'FIELD-003877-ALPHA'.
               10  SCB-D-003878             PIC 9(04) VALUE 7146.
               10  SCB-DESC-003878          PIC X(18) VALUE 'DESC-050414'.
               10  SCB-E-003879             OCCURS 5 TIMES.
                   15  SCB-EK-003879        PIC X(10).
                   15  SCB-EV-003879        PIC 9(06) COMP.
               10  SCB-A-003880             PIC 9(09) COMP VALUE 3880.
           05  SCB-GRP-0354.
               10  SCB-B-003881             PIC S9(07)V99 COMP-3 VALUE +0003881.
               10  SCB-C-003882             PIC X(24) VALUE 'FIELD-003882-ALPHA'.
               10  SCB-D-003883             PIC 9(04) VALUE 7181.
               10  SCB-DESC-003883          PIC X(18) VALUE 'DESC-050479'.
               10  SCB-E-003884             OCCURS 2 TIMES.
                   15  SCB-EK-003884        PIC X(10).
                   15  SCB-EV-003884        PIC 9(06) COMP.
               10  SCB-A-003885             PIC 9(09) COMP VALUE 3885.
               10  SCB-B-003886             PIC S9(07)V99 COMP-3 VALUE +0003886.
               10  SCB-C-003887             PIC X(24) VALUE 'FIELD-003887-ALPHA'.
               10  SCB-D-003888             PIC 9(04) VALUE 7216.
               10  SCB-DESC-003888          PIC X(18) VALUE 'DESC-050544'.
               10  SCB-E-003889             OCCURS 3 TIMES.
                   15  SCB-EK-003889        PIC X(10).
                   15  SCB-EV-003889        PIC 9(06) COMP.
               10  SCB-A-003890             PIC 9(09) COMP VALUE 3890.
               10  SCB-B-003891             PIC S9(07)V99 COMP-3 VALUE +0003891.
               10  SCB-C-003892             PIC X(24) VALUE 'FIELD-003892-ALPHA'.
           05  SCB-ALT-0059 REDEFINES SCB-GRP-0354.
               10  SCB-ALT-FLAG-0059      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0059   PIC X(64).
           05  SCB-GRP-0355.
               10  SCB-D-003893             PIC 9(04) VALUE 7251.
               10  SCB-DESC-003893          PIC X(18) VALUE 'DESC-050609'.
               10  SCB-E-003894             OCCURS 4 TIMES.
                   15  SCB-EK-003894        PIC X(10).
                   15  SCB-EV-003894        PIC 9(06) COMP.
               10  SCB-A-003895             PIC 9(09) COMP VALUE 3895.
               10  SCB-B-003896             PIC S9(07)V99 COMP-3 VALUE +0003896.
               10  SCB-C-003897             PIC X(24) VALUE 'FIELD-003897-ALPHA'.
               10  SCB-D-003898             PIC 9(04) VALUE 7286.
               10  SCB-DESC-003898          PIC X(18) VALUE 'DESC-050674'.
               10  SCB-E-003899             OCCURS 5 TIMES.
                   15  SCB-EK-003899        PIC X(10).
                   15  SCB-EV-003899        PIC 9(06) COMP.
               10  SCB-A-003900             PIC 9(09) COMP VALUE 3900.
               10  SCB-B-003901             PIC S9(07)V99 COMP-3 VALUE +0003901.
               10  SCB-C-003902             PIC X(24) VALUE 'FIELD-003902-ALPHA'.
               10  SCB-D-003903             PIC 9(04) VALUE 7321.
               10  SCB-DESC-003903          PIC X(18) VALUE 'DESC-050739'.
               10  SCB-E-003904             OCCURS 2 TIMES.
                   15  SCB-EK-003904        PIC X(10).
                   15  SCB-EV-003904        PIC 9(06) COMP.
               10  SCB-A-003905             PIC 9(09) COMP VALUE 3905.
           05  SCB-GRP-0356.
               10  SCB-B-003906             PIC S9(07)V99 COMP-3 VALUE +0003906.
               10  SCB-C-003907             PIC X(24) VALUE 'FIELD-003907-ALPHA'.
               10  SCB-D-003908             PIC 9(04) VALUE 7356.
               10  SCB-DESC-003908          PIC X(18) VALUE 'DESC-050804'.
               10  SCB-E-003909             OCCURS 3 TIMES.
                   15  SCB-EK-003909        PIC X(10).
                   15  SCB-EV-003909        PIC 9(06) COMP.
               10  SCB-A-003910             PIC 9(09) COMP VALUE 3910.
               10  SCB-B-003911             PIC S9(07)V99 COMP-3 VALUE +0003911.
               10  SCB-C-003912             PIC X(24) VALUE 'FIELD-003912-ALPHA'.
               10  SCB-D-003913             PIC 9(04) VALUE 7391.
               10  SCB-DESC-003913          PIC X(18) VALUE 'DESC-050869'.
               10  SCB-E-003914             OCCURS 4 TIMES.
                   15  SCB-EK-003914        PIC X(10).
                   15  SCB-EV-003914        PIC 9(06) COMP.
               10  SCB-A-003915             PIC 9(09) COMP VALUE 3915.
               10  SCB-B-003916             PIC S9(07)V99 COMP-3 VALUE +0003916.
               10  SCB-C-003917             PIC X(24) VALUE 'FIELD-003917-ALPHA'.
               10  SCB-D-003918             PIC 9(04) VALUE 7426.
               10  SCB-DESC-003918          PIC X(18) VALUE 'DESC-050934'.
               10  SCB-E-003919             OCCURS 5 TIMES.
                   15  SCB-EK-003919        PIC X(10).
                   15  SCB-EV-003919        PIC 9(06) COMP.
           05  SCB-GRP-0357.
               10  SCB-A-003920             PIC 9(09) COMP VALUE 3920.
               10  SCB-B-003921             PIC S9(07)V99 COMP-3 VALUE +0003921.
               10  SCB-C-003922             PIC X(24) VALUE 'FIELD-003922-ALPHA'.
               10  SCB-D-003923             PIC 9(04) VALUE 7461.
               10  SCB-DESC-003923          PIC X(18) VALUE 'DESC-050999'.
               10  SCB-E-003924             OCCURS 2 TIMES.
                   15  SCB-EK-003924        PIC X(10).
                   15  SCB-EV-003924        PIC 9(06) COMP.
               10  SCB-A-003925             PIC 9(09) COMP VALUE 3925.
               10  SCB-B-003926             PIC S9(07)V99 COMP-3 VALUE +0003926.
               10  SCB-C-003927             PIC X(24) VALUE 'FIELD-003927-ALPHA'.
           05  SCB-GRP-0358.
               10  SCB-D-003928             PIC 9(04) VALUE 7496.
               10  SCB-DESC-003928          PIC X(18) VALUE 'DESC-051064'.
               10  SCB-E-003929             OCCURS 3 TIMES.
                   15  SCB-EK-003929        PIC X(10).
                   15  SCB-EV-003929        PIC 9(06) COMP.
               10  SCB-A-003930             PIC 9(09) COMP VALUE 3930.
               10  SCB-B-003931             PIC S9(07)V99 COMP-3 VALUE +0003931.
               10  SCB-C-003932             PIC X(24) VALUE 'FIELD-003932-ALPHA'.
               10  SCB-D-003933             PIC 9(04) VALUE 7531.
               10  SCB-DESC-003933          PIC X(18) VALUE 'DESC-051129'.
               10  SCB-E-003934             OCCURS 4 TIMES.
                   15  SCB-EK-003934        PIC X(10).
                   15  SCB-EV-003934        PIC 9(06) COMP.
               10  SCB-A-003935             PIC 9(09) COMP VALUE 3935.
               10  SCB-B-003936             PIC S9(07)V99 COMP-3 VALUE +0003936.
           05  SCB-GRP-0359.
               10  SCB-C-003937             PIC X(24) VALUE 'FIELD-003937-ALPHA'.
               10  SCB-D-003938             PIC 9(04) VALUE 7566.
               10  SCB-DESC-003938          PIC X(18) VALUE 'DESC-051194'.
               10  SCB-E-003939             OCCURS 5 TIMES.
                   15  SCB-EK-003939        PIC X(10).
                   15  SCB-EV-003939        PIC 9(06) COMP.
               10  SCB-A-003940             PIC 9(09) COMP VALUE 3940.
               10  SCB-B-003941             PIC S9(07)V99 COMP-3 VALUE +0003941.
               10  SCB-C-003942             PIC X(24) VALUE 'FIELD-003942-ALPHA'.
               10  SCB-D-003943             PIC 9(04) VALUE 7601.
               10  SCB-DESC-003943          PIC X(18) VALUE 'DESC-051259'.
               10  SCB-E-003944             OCCURS 2 TIMES.
                   15  SCB-EK-003944        PIC X(10).
                   15  SCB-EV-003944        PIC 9(06) COMP.
               10  SCB-A-003945             PIC 9(09) COMP VALUE 3945.
               10  SCB-B-003946             PIC S9(07)V99 COMP-3 VALUE +0003946.
           05  SCB-GRP-0360.
               10  SCB-C-003947             PIC X(24) VALUE 'FIELD-003947-ALPHA'.
               10  SCB-D-003948             PIC 9(04) VALUE 7636.
               10  SCB-DESC-003948          PIC X(18) VALUE 'DESC-051324'.
               10  SCB-E-003949             OCCURS 3 TIMES.
                   15  SCB-EK-003949        PIC X(10).
                   15  SCB-EV-003949        PIC 9(06) COMP.
               10  SCB-A-003950             PIC 9(09) COMP VALUE 3950.
               10  SCB-B-003951             PIC S9(07)V99 COMP-3 VALUE +0003951.
               10  SCB-C-003952             PIC X(24) VALUE 'FIELD-003952-ALPHA'.
               10  SCB-D-003953             PIC 9(04) VALUE 7671.
               10  SCB-DESC-003953          PIC X(18) VALUE 'DESC-051389'.
               10  SCB-E-003954             OCCURS 4 TIMES.
                   15  SCB-EK-003954        PIC X(10).
                   15  SCB-EV-003954        PIC 9(06) COMP.
               10  SCB-A-003955             PIC 9(09) COMP VALUE 3955.
               10  SCB-B-003956             PIC S9(07)V99 COMP-3 VALUE +0003956.
               10  SCB-C-003957             PIC X(24) VALUE 'FIELD-003957-ALPHA'.
           05  SCB-ALT-0060 REDEFINES SCB-GRP-0360.
               10  SCB-ALT-FLAG-0060      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0060   PIC X(64).
           05  SCB-GRP-0361.
               10  SCB-D-003958             PIC 9(04) VALUE 7706.
               10  SCB-DESC-003958          PIC X(18) VALUE 'DESC-051454'.
               10  SCB-E-003959             OCCURS 5 TIMES.
                   15  SCB-EK-003959        PIC X(10).
                   15  SCB-EV-003959        PIC 9(06) COMP.
               10  SCB-A-003960             PIC 9(09) COMP VALUE 3960.
               10  SCB-B-003961             PIC S9(07)V99 COMP-3 VALUE +0003961.
               10  SCB-C-003962             PIC X(24) VALUE 'FIELD-003962-ALPHA'.
               10  SCB-D-003963             PIC 9(04) VALUE 7741.
               10  SCB-DESC-003963          PIC X(18) VALUE 'DESC-051519'.
               10  SCB-E-003964             OCCURS 2 TIMES.
                   15  SCB-EK-003964        PIC X(10).
                   15  SCB-EV-003964        PIC 9(06) COMP.
               10  SCB-A-003965             PIC 9(09) COMP VALUE 3965.
               10  SCB-B-003966             PIC S9(07)V99 COMP-3 VALUE +0003966.
               10  SCB-C-003967             PIC X(24) VALUE 'FIELD-003967-ALPHA'.
               10  SCB-D-003968             PIC 9(04) VALUE 7776.
               10  SCB-DESC-003968          PIC X(18) VALUE 'DESC-051584'.
               10  SCB-E-003969             OCCURS 3 TIMES.
                   15  SCB-EK-003969        PIC X(10).
                   15  SCB-EV-003969        PIC 9(06) COMP.
           05  SCB-GRP-0362.
               10  SCB-A-003970             PIC 9(09) COMP VALUE 3970.
               10  SCB-B-003971             PIC S9(07)V99 COMP-3 VALUE +0003971.
               10  SCB-C-003972             PIC X(24) VALUE 'FIELD-003972-ALPHA'.
               10  SCB-D-003973             PIC 9(04) VALUE 7811.
               10  SCB-DESC-003973          PIC X(18) VALUE 'DESC-051649'.
               10  SCB-E-003974             OCCURS 4 TIMES.
                   15  SCB-EK-003974        PIC X(10).
                   15  SCB-EV-003974        PIC 9(06) COMP.
               10  SCB-A-003975             PIC 9(09) COMP VALUE 3975.
               10  SCB-B-003976             PIC S9(07)V99 COMP-3 VALUE +0003976.
               10  SCB-C-003977             PIC X(24) VALUE 'FIELD-003977-ALPHA'.
               10  SCB-D-003978             PIC 9(04) VALUE 7846.
               10  SCB-DESC-003978          PIC X(18) VALUE 'DESC-051714'.
               10  SCB-E-003979             OCCURS 5 TIMES.
                   15  SCB-EK-003979        PIC X(10).
                   15  SCB-EV-003979        PIC 9(06) COMP.
               10  SCB-A-003980             PIC 9(09) COMP VALUE 3980.
               10  SCB-B-003981             PIC S9(07)V99 COMP-3 VALUE +0003981.
               10  SCB-C-003982             PIC X(24) VALUE 'FIELD-003982-ALPHA'.
           05  SCB-GRP-0363.
               10  SCB-D-003983             PIC 9(04) VALUE 7881.
               10  SCB-DESC-003983          PIC X(18) VALUE 'DESC-051779'.
               10  SCB-E-003984             OCCURS 2 TIMES.
                   15  SCB-EK-003984        PIC X(10).
                   15  SCB-EV-003984        PIC 9(06) COMP.
               10  SCB-A-003985             PIC 9(09) COMP VALUE 3985.
               10  SCB-B-003986             PIC S9(07)V99 COMP-3 VALUE +0003986.
               10  SCB-C-003987             PIC X(24) VALUE 'FIELD-003987-ALPHA'.
               10  SCB-D-003988             PIC 9(04) VALUE 7916.
               10  SCB-DESC-003988          PIC X(18) VALUE 'DESC-051844'.
               10  SCB-E-003989             OCCURS 3 TIMES.
                   15  SCB-EK-003989        PIC X(10).
                   15  SCB-EV-003989        PIC 9(06) COMP.
               10  SCB-A-003990             PIC 9(09) COMP VALUE 3990.
               10  SCB-B-003991             PIC S9(07)V99 COMP-3 VALUE +0003991.
               10  SCB-C-003992             PIC X(24) VALUE 'FIELD-003992-ALPHA'.
               10  SCB-D-003993             PIC 9(04) VALUE 7951.
               10  SCB-DESC-003993          PIC X(18) VALUE 'DESC-051909'.
               10  SCB-E-003994             OCCURS 4 TIMES.
                   15  SCB-EK-003994        PIC X(10).
                   15  SCB-EV-003994        PIC 9(06) COMP.
               10  SCB-A-003995             PIC 9(09) COMP VALUE 3995.
               10  SCB-B-003996             PIC S9(07)V99 COMP-3 VALUE +0003996.
           05  SCB-GRP-0364.
               10  SCB-C-003997             PIC X(24) VALUE 'FIELD-003997-ALPHA'.
               10  SCB-D-003998             PIC 9(04) VALUE 7986.
               10  SCB-DESC-003998          PIC X(18) VALUE 'DESC-051974'.
               10  SCB-E-003999             OCCURS 5 TIMES.
                   15  SCB-EK-003999        PIC X(10).
                   15  SCB-EV-003999        PIC 9(06) COMP.
               10  SCB-A-004000             PIC 9(09) COMP VALUE 4000.
               10  SCB-B-004001             PIC S9(07)V99 COMP-3 VALUE +0004001.
               10  SCB-C-004002             PIC X(24) VALUE 'FIELD-004002-ALPHA'.
               10  SCB-D-004003             PIC 9(04) VALUE 8021.
               10  SCB-DESC-004003          PIC X(18) VALUE 'DESC-052039'.
               10  SCB-E-004004             OCCURS 2 TIMES.
                   15  SCB-EK-004004        PIC X(10).
                   15  SCB-EV-004004        PIC 9(06) COMP.
           05  SCB-GRP-0365.
               10  SCB-A-004005             PIC 9(09) COMP VALUE 4005.
               10  SCB-B-004006             PIC S9(07)V99 COMP-3 VALUE +0004006.
               10  SCB-C-004007             PIC X(24) VALUE 'FIELD-004007-ALPHA'.
               10  SCB-D-004008             PIC 9(04) VALUE 8056.
               10  SCB-DESC-004008          PIC X(18) VALUE 'DESC-052104'.
               10  SCB-E-004009             OCCURS 3 TIMES.
                   15  SCB-EK-004009        PIC X(10).
                   15  SCB-EV-004009        PIC 9(06) COMP.
               10  SCB-A-004010             PIC 9(09) COMP VALUE 4010.
               10  SCB-B-004011             PIC S9(07)V99 COMP-3 VALUE +0004011.
               10  SCB-C-004012             PIC X(24) VALUE 'FIELD-004012-ALPHA'.
               10  SCB-D-004013             PIC 9(04) VALUE 8091.
               10  SCB-DESC-004013          PIC X(18) VALUE 'DESC-052169'.
           05  SCB-GRP-0366.
               10  SCB-E-004014             OCCURS 4 TIMES.
                   15  SCB-EK-004014        PIC X(10).
                   15  SCB-EV-004014        PIC 9(06) COMP.
               10  SCB-A-004015             PIC 9(09) COMP VALUE 4015.
               10  SCB-B-004016             PIC S9(07)V99 COMP-3 VALUE +0004016.
               10  SCB-C-004017             PIC X(24) VALUE 'FIELD-004017-ALPHA'.
               10  SCB-D-004018             PIC 9(04) VALUE 8126.
               10  SCB-DESC-004018          PIC X(18) VALUE 'DESC-052234'.
               10  SCB-E-004019             OCCURS 5 TIMES.
                   15  SCB-EK-004019        PIC X(10).
                   15  SCB-EV-004019        PIC 9(06) COMP.
               10  SCB-A-004020             PIC 9(09) COMP VALUE 4020.
               10  SCB-B-004021             PIC S9(07)V99 COMP-3 VALUE +0004021.
               10  SCB-C-004022             PIC X(24) VALUE 'FIELD-004022-ALPHA'.
               10  SCB-D-004023             PIC 9(04) VALUE 8161.
               10  SCB-DESC-004023          PIC X(18) VALUE 'DESC-052299'.
           05  SCB-ALT-0061 REDEFINES SCB-GRP-0366.
               10  SCB-ALT-FLAG-0061      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0061   PIC X(64).
           05  SCB-GRP-0367.
               10  SCB-E-004024             OCCURS 2 TIMES.
                   15  SCB-EK-004024        PIC X(10).
                   15  SCB-EV-004024        PIC 9(06) COMP.
               10  SCB-A-004025             PIC 9(09) COMP VALUE 4025.
               10  SCB-B-004026             PIC S9(07)V99 COMP-3 VALUE +0004026.
               10  SCB-C-004027             PIC X(24) VALUE 'FIELD-004027-ALPHA'.
               10  SCB-D-004028             PIC 9(04) VALUE 8196.
               10  SCB-DESC-004028          PIC X(18) VALUE 'DESC-052364'.
               10  SCB-E-004029             OCCURS 3 TIMES.
                   15  SCB-EK-004029        PIC X(10).
                   15  SCB-EV-004029        PIC 9(06) COMP.
               10  SCB-A-004030             PIC 9(09) COMP VALUE 4030.
               10  SCB-B-004031             PIC S9(07)V99 COMP-3 VALUE +0004031.
               10  SCB-C-004032             PIC X(24) VALUE 'FIELD-004032-ALPHA'.
               10  SCB-D-004033             PIC 9(04) VALUE 8231.
               10  SCB-DESC-004033          PIC X(18) VALUE 'DESC-052429'.
               10  SCB-E-004034             OCCURS 4 TIMES.
                   15  SCB-EK-004034        PIC X(10).
                   15  SCB-EV-004034        PIC 9(06) COMP.
           05  SCB-GRP-0368.
               10  SCB-A-004035             PIC 9(09) COMP VALUE 4035.
               10  SCB-B-004036             PIC S9(07)V99 COMP-3 VALUE +0004036.
               10  SCB-C-004037             PIC X(24) VALUE 'FIELD-004037-ALPHA'.
               10  SCB-D-004038             PIC 9(04) VALUE 8266.
               10  SCB-DESC-004038          PIC X(18) VALUE 'DESC-052494'.
               10  SCB-E-004039             OCCURS 5 TIMES.
                   15  SCB-EK-004039        PIC X(10).
                   15  SCB-EV-004039        PIC 9(06) COMP.
               10  SCB-A-004040             PIC 9(09) COMP VALUE 4040.
               10  SCB-B-004041             PIC S9(07)V99 COMP-3 VALUE +0004041.
               10  SCB-C-004042             PIC X(24) VALUE 'FIELD-004042-ALPHA'.
               10  SCB-D-004043             PIC 9(04) VALUE 8301.
               10  SCB-DESC-004043          PIC X(18) VALUE 'DESC-052559'.
               10  SCB-E-004044             OCCURS 2 TIMES.
                   15  SCB-EK-004044        PIC X(10).
                   15  SCB-EV-004044        PIC 9(06) COMP.
               10  SCB-A-004045             PIC 9(09) COMP VALUE 4045.
               10  SCB-B-004046             PIC S9(07)V99 COMP-3 VALUE +0004046.
           05  SCB-GRP-0369.
               10  SCB-C-004047             PIC X(24) VALUE 'FIELD-004047-ALPHA'.
               10  SCB-D-004048             PIC 9(04) VALUE 8336.
               10  SCB-DESC-004048          PIC X(18) VALUE 'DESC-052624'.
               10  SCB-E-004049             OCCURS 3 TIMES.
                   15  SCB-EK-004049        PIC X(10).
                   15  SCB-EV-004049        PIC 9(06) COMP.
               10  SCB-A-004050             PIC 9(09) COMP VALUE 4050.
               10  SCB-B-004051             PIC S9(07)V99 COMP-3 VALUE +0004051.
               10  SCB-C-004052             PIC X(24) VALUE 'FIELD-004052-ALPHA'.
               10  SCB-D-004053             PIC 9(04) VALUE 8371.
               10  SCB-DESC-004053          PIC X(18) VALUE 'DESC-052689'.
               10  SCB-E-004054             OCCURS 4 TIMES.
                   15  SCB-EK-004054        PIC X(10).
                   15  SCB-EV-004054        PIC 9(06) COMP.
               10  SCB-A-004055             PIC 9(09) COMP VALUE 4055.
               10  SCB-B-004056             PIC S9(07)V99 COMP-3 VALUE +0004056.
               10  SCB-C-004057             PIC X(24) VALUE 'FIELD-004057-ALPHA'.
               10  SCB-D-004058             PIC 9(04) VALUE 8406.
               10  SCB-DESC-004058          PIC X(18) VALUE 'DESC-052754'.
               10  SCB-E-004059             OCCURS 5 TIMES.
                   15  SCB-EK-004059        PIC X(10).
                   15  SCB-EV-004059        PIC 9(06) COMP.
           05  SCB-GRP-0370.
               10  SCB-A-004060             PIC 9(09) COMP VALUE 4060.
               10  SCB-B-004061             PIC S9(07)V99 COMP-3 VALUE +0004061.
               10  SCB-C-004062             PIC X(24) VALUE 'FIELD-004062-ALPHA'.
               10  SCB-D-004063             PIC 9(04) VALUE 8441.
               10  SCB-DESC-004063          PIC X(18) VALUE 'DESC-052819'.
               10  SCB-E-004064             OCCURS 2 TIMES.
                   15  SCB-EK-004064        PIC X(10).
                   15  SCB-EV-004064        PIC 9(06) COMP.
               10  SCB-A-004065             PIC 9(09) COMP VALUE 4065.
               10  SCB-B-004066             PIC S9(07)V99 COMP-3 VALUE +0004066.
               10  SCB-C-004067             PIC X(24) VALUE 'FIELD-004067-ALPHA'.
               10  SCB-D-004068             PIC 9(04) VALUE 8476.
               10  SCB-DESC-004068          PIC X(18) VALUE 'DESC-052884'.
               10  SCB-E-004069             OCCURS 3 TIMES.
                   15  SCB-EK-004069        PIC X(10).
                   15  SCB-EV-004069        PIC 9(06) COMP.
               10  SCB-A-004070             PIC 9(09) COMP VALUE 4070.
               10  SCB-B-004071             PIC S9(07)V99 COMP-3 VALUE +0004071.
               10  SCB-C-004072             PIC X(24) VALUE 'FIELD-004072-ALPHA'.
               10  SCB-D-004073             PIC 9(04) VALUE 8511.
               10  SCB-DESC-004073          PIC X(18) VALUE 'DESC-052949'.
           05  SCB-GRP-0371.
               10  SCB-E-004074             OCCURS 4 TIMES.
                   15  SCB-EK-004074        PIC X(10).
                   15  SCB-EV-004074        PIC 9(06) COMP.
               10  SCB-A-004075             PIC 9(09) COMP VALUE 4075.
               10  SCB-B-004076             PIC S9(07)V99 COMP-3 VALUE +0004076.
               10  SCB-C-004077             PIC X(24) VALUE 'FIELD-004077-ALPHA'.
               10  SCB-D-004078             PIC 9(04) VALUE 8546.
               10  SCB-DESC-004078          PIC X(18) VALUE 'DESC-053014'.
               10  SCB-E-004079             OCCURS 5 TIMES.
                   15  SCB-EK-004079        PIC X(10).
                   15  SCB-EV-004079        PIC 9(06) COMP.
               10  SCB-A-004080             PIC 9(09) COMP VALUE 4080.
               10  SCB-B-004081             PIC S9(07)V99 COMP-3 VALUE +0004081.
           05  SCB-GRP-0372.
               10  SCB-C-004082             PIC X(24) VALUE 'FIELD-004082-ALPHA'.
               10  SCB-D-004083             PIC 9(04) VALUE 8581.
               10  SCB-DESC-004083          PIC X(18) VALUE 'DESC-053079'.
               10  SCB-E-004084             OCCURS 2 TIMES.
                   15  SCB-EK-004084        PIC X(10).
                   15  SCB-EV-004084        PIC 9(06) COMP.
               10  SCB-A-004085             PIC 9(09) COMP VALUE 4085.
               10  SCB-B-004086             PIC S9(07)V99 COMP-3 VALUE +0004086.
               10  SCB-C-004087             PIC X(24) VALUE 'FIELD-004087-ALPHA'.
               10  SCB-D-004088             PIC 9(04) VALUE 8616.
               10  SCB-DESC-004088          PIC X(18) VALUE 'DESC-053144'.
               10  SCB-E-004089             OCCURS 3 TIMES.
                   15  SCB-EK-004089        PIC X(10).
                   15  SCB-EV-004089        PIC 9(06) COMP.
               10  SCB-A-004090             PIC 9(09) COMP VALUE 4090.
           05  SCB-ALT-0062 REDEFINES SCB-GRP-0372.
               10  SCB-ALT-FLAG-0062      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0062   PIC X(64).
           05  SCB-GRP-0373.
               10  SCB-B-004091             PIC S9(07)V99 COMP-3 VALUE +0004091.
               10  SCB-C-004092             PIC X(24) VALUE 'FIELD-004092-ALPHA'.
               10  SCB-D-004093             PIC 9(04) VALUE 8651.
               10  SCB-DESC-004093          PIC X(18) VALUE 'DESC-053209'.
               10  SCB-E-004094             OCCURS 4 TIMES.
                   15  SCB-EK-004094        PIC X(10).
                   15  SCB-EV-004094        PIC 9(06) COMP.
               10  SCB-A-004095             PIC 9(09) COMP VALUE 4095.
               10  SCB-B-004096             PIC S9(07)V99 COMP-3 VALUE +0004096.
               10  SCB-C-004097             PIC X(24) VALUE 'FIELD-004097-ALPHA'.
               10  SCB-D-004098             PIC 9(04) VALUE 8686.
               10  SCB-DESC-004098          PIC X(18) VALUE 'DESC-053274'.
               10  SCB-E-004099             OCCURS 5 TIMES.
                   15  SCB-EK-004099        PIC X(10).
                   15  SCB-EV-004099        PIC 9(06) COMP.
               10  SCB-A-004100             PIC 9(09) COMP VALUE 4100.
           05  SCB-GRP-0374.
               10  SCB-B-004101             PIC S9(07)V99 COMP-3 VALUE +0004101.
               10  SCB-C-004102             PIC X(24) VALUE 'FIELD-004102-ALPHA'.
               10  SCB-D-004103             PIC 9(04) VALUE 8721.
               10  SCB-DESC-004103          PIC X(18) VALUE 'DESC-053339'.
               10  SCB-E-004104             OCCURS 2 TIMES.
                   15  SCB-EK-004104        PIC X(10).
                   15  SCB-EV-004104        PIC 9(06) COMP.
               10  SCB-A-004105             PIC 9(09) COMP VALUE 4105.
               10  SCB-B-004106             PIC S9(07)V99 COMP-3 VALUE +0004106.
               10  SCB-C-004107             PIC X(24) VALUE 'FIELD-004107-ALPHA'.
               10  SCB-D-004108             PIC 9(04) VALUE 8756.
               10  SCB-DESC-004108          PIC X(18) VALUE 'DESC-053404'.
               10  SCB-E-004109             OCCURS 3 TIMES.
                   15  SCB-EK-004109        PIC X(10).
                   15  SCB-EV-004109        PIC 9(06) COMP.
               10  SCB-A-004110             PIC 9(09) COMP VALUE 4110.
               10  SCB-B-004111             PIC S9(07)V99 COMP-3 VALUE +0004111.
           05  SCB-GRP-0375.
               10  SCB-C-004112             PIC X(24) VALUE 'FIELD-004112-ALPHA'.
               10  SCB-D-004113             PIC 9(04) VALUE 8791.
               10  SCB-DESC-004113          PIC X(18) VALUE 'DESC-053469'.
               10  SCB-E-004114             OCCURS 4 TIMES.
                   15  SCB-EK-004114        PIC X(10).
                   15  SCB-EV-004114        PIC 9(06) COMP.
               10  SCB-A-004115             PIC 9(09) COMP VALUE 4115.
               10  SCB-B-004116             PIC S9(07)V99 COMP-3 VALUE +0004116.
               10  SCB-C-004117             PIC X(24) VALUE 'FIELD-004117-ALPHA'.
               10  SCB-D-004118             PIC 9(04) VALUE 8826.
               10  SCB-DESC-004118          PIC X(18) VALUE 'DESC-053534'.
               10  SCB-E-004119             OCCURS 5 TIMES.
                   15  SCB-EK-004119        PIC X(10).
                   15  SCB-EV-004119        PIC 9(06) COMP.
               10  SCB-A-004120             PIC 9(09) COMP VALUE 4120.
               10  SCB-B-004121             PIC S9(07)V99 COMP-3 VALUE +0004121.
               10  SCB-C-004122             PIC X(24) VALUE 'FIELD-004122-ALPHA'.
               10  SCB-D-004123             PIC 9(04) VALUE 8861.
               10  SCB-DESC-004123          PIC X(18) VALUE 'DESC-053599'.
           05  SCB-GRP-0376.
               10  SCB-E-004124             OCCURS 2 TIMES.
                   15  SCB-EK-004124        PIC X(10).
                   15  SCB-EV-004124        PIC 9(06) COMP.
               10  SCB-A-004125             PIC 9(09) COMP VALUE 4125.
               10  SCB-B-004126             PIC S9(07)V99 COMP-3 VALUE +0004126.
               10  SCB-C-004127             PIC X(24) VALUE 'FIELD-004127-ALPHA'.
               10  SCB-D-004128             PIC 9(04) VALUE 8896.
               10  SCB-DESC-004128          PIC X(18) VALUE 'DESC-053664'.
               10  SCB-E-004129             OCCURS 3 TIMES.
                   15  SCB-EK-004129        PIC X(10).
                   15  SCB-EV-004129        PIC 9(06) COMP.
               10  SCB-A-004130             PIC 9(09) COMP VALUE 4130.
               10  SCB-B-004131             PIC S9(07)V99 COMP-3 VALUE +0004131.
               10  SCB-C-004132             PIC X(24) VALUE 'FIELD-004132-ALPHA'.
               10  SCB-D-004133             PIC 9(04) VALUE 8931.
               10  SCB-DESC-004133          PIC X(18) VALUE 'DESC-053729'.
               10  SCB-E-004134             OCCURS 4 TIMES.
                   15  SCB-EK-004134        PIC X(10).
                   15  SCB-EV-004134        PIC 9(06) COMP.
               10  SCB-A-004135             PIC 9(09) COMP VALUE 4135.
               10  SCB-B-004136             PIC S9(07)V99 COMP-3 VALUE +0004136.
           05  SCB-GRP-0377.
               10  SCB-C-004137             PIC X(24) VALUE 'FIELD-004137-ALPHA'.
               10  SCB-D-004138             PIC 9(04) VALUE 8966.
               10  SCB-DESC-004138          PIC X(18) VALUE 'DESC-053794'.
               10  SCB-E-004139             OCCURS 5 TIMES.
                   15  SCB-EK-004139        PIC X(10).
                   15  SCB-EV-004139        PIC 9(06) COMP.
               10  SCB-A-004140             PIC 9(09) COMP VALUE 4140.
               10  SCB-B-004141             PIC S9(07)V99 COMP-3 VALUE +0004141.
               10  SCB-C-004142             PIC X(24) VALUE 'FIELD-004142-ALPHA'.
               10  SCB-D-004143             PIC 9(04) VALUE 9001.
               10  SCB-DESC-004143          PIC X(18) VALUE 'DESC-053859'.
               10  SCB-E-004144             OCCURS 2 TIMES.
                   15  SCB-EK-004144        PIC X(10).
                   15  SCB-EV-004144        PIC 9(06) COMP.
               10  SCB-A-004145             PIC 9(09) COMP VALUE 4145.
               10  SCB-B-004146             PIC S9(07)V99 COMP-3 VALUE +0004146.
               10  SCB-C-004147             PIC X(24) VALUE 'FIELD-004147-ALPHA'.
               10  SCB-D-004148             PIC 9(04) VALUE 9036.
               10  SCB-DESC-004148          PIC X(18) VALUE 'DESC-053924'.
               10  SCB-E-004149             OCCURS 3 TIMES.
                   15  SCB-EK-004149        PIC X(10).
                   15  SCB-EV-004149        PIC 9(06) COMP.
               10  SCB-A-004150             PIC 9(09) COMP VALUE 4150.
           05  SCB-GRP-0378.
               10  SCB-B-004151             PIC S9(07)V99 COMP-3 VALUE +0004151.
               10  SCB-C-004152             PIC X(24) VALUE 'FIELD-004152-ALPHA'.
               10  SCB-D-004153             PIC 9(04) VALUE 9071.
               10  SCB-DESC-004153          PIC X(18) VALUE 'DESC-053989'.
               10  SCB-E-004154             OCCURS 4 TIMES.
                   15  SCB-EK-004154        PIC X(10).
                   15  SCB-EV-004154        PIC 9(06) COMP.
               10  SCB-A-004155             PIC 9(09) COMP VALUE 4155.
               10  SCB-B-004156             PIC S9(07)V99 COMP-3 VALUE +0004156.
               10  SCB-C-004157             PIC X(24) VALUE 'FIELD-004157-ALPHA'.
               10  SCB-D-004158             PIC 9(04) VALUE 9106.
               10  SCB-DESC-004158          PIC X(18) VALUE 'DESC-054054'.
           05  SCB-ALT-0063 REDEFINES SCB-GRP-0378.
               10  SCB-ALT-FLAG-0063      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0063   PIC X(64).
           05  SCB-GRP-0379.
               10  SCB-E-004159             OCCURS 5 TIMES.
                   15  SCB-EK-004159        PIC X(10).
                   15  SCB-EV-004159        PIC 9(06) COMP.
               10  SCB-A-004160             PIC 9(09) COMP VALUE 4160.
               10  SCB-B-004161             PIC S9(07)V99 COMP-3 VALUE +0004161.
               10  SCB-C-004162             PIC X(24) VALUE 'FIELD-004162-ALPHA'.
               10  SCB-D-004163             PIC 9(04) VALUE 9141.
               10  SCB-DESC-004163          PIC X(18) VALUE 'DESC-054119'.
               10  SCB-E-004164             OCCURS 2 TIMES.
                   15  SCB-EK-004164        PIC X(10).
                   15  SCB-EV-004164        PIC 9(06) COMP.
               10  SCB-A-004165             PIC 9(09) COMP VALUE 4165.
               10  SCB-B-004166             PIC S9(07)V99 COMP-3 VALUE +0004166.
               10  SCB-C-004167             PIC X(24) VALUE 'FIELD-004167-ALPHA'.
           05  SCB-GRP-0380.
               10  SCB-D-004168             PIC 9(04) VALUE 9176.
               10  SCB-DESC-004168          PIC X(18) VALUE 'DESC-054184'.
               10  SCB-E-004169             OCCURS 3 TIMES.
                   15  SCB-EK-004169        PIC X(10).
                   15  SCB-EV-004169        PIC 9(06) COMP.
               10  SCB-A-004170             PIC 9(09) COMP VALUE 4170.
               10  SCB-B-004171             PIC S9(07)V99 COMP-3 VALUE +0004171.
               10  SCB-C-004172             PIC X(24) VALUE 'FIELD-004172-ALPHA'.
               10  SCB-D-004173             PIC 9(04) VALUE 9211.
               10  SCB-DESC-004173          PIC X(18) VALUE 'DESC-054249'.
               10  SCB-E-004174             OCCURS 4 TIMES.
                   15  SCB-EK-004174        PIC X(10).
                   15  SCB-EV-004174        PIC 9(06) COMP.
               10  SCB-A-004175             PIC 9(09) COMP VALUE 4175.
               10  SCB-B-004176             PIC S9(07)V99 COMP-3 VALUE +0004176.
               10  SCB-C-004177             PIC X(24) VALUE 'FIELD-004177-ALPHA'.
           05  SCB-GRP-0381.
               10  SCB-D-004178             PIC 9(04) VALUE 9246.
               10  SCB-DESC-004178          PIC X(18) VALUE 'DESC-054314'.
               10  SCB-E-004179             OCCURS 5 TIMES.
                   15  SCB-EK-004179        PIC X(10).
                   15  SCB-EV-004179        PIC 9(06) COMP.
               10  SCB-A-004180             PIC 9(09) COMP VALUE 4180.
               10  SCB-B-004181             PIC S9(07)V99 COMP-3 VALUE +0004181.
               10  SCB-C-004182             PIC X(24) VALUE 'FIELD-004182-ALPHA'.
               10  SCB-D-004183             PIC 9(04) VALUE 9281.
               10  SCB-DESC-004183          PIC X(18) VALUE 'DESC-054379'.
               10  SCB-E-004184             OCCURS 2 TIMES.
                   15  SCB-EK-004184        PIC X(10).
                   15  SCB-EV-004184        PIC 9(06) COMP.
               10  SCB-A-004185             PIC 9(09) COMP VALUE 4185.
               10  SCB-B-004186             PIC S9(07)V99 COMP-3 VALUE +0004186.
               10  SCB-C-004187             PIC X(24) VALUE 'FIELD-004187-ALPHA'.
               10  SCB-D-004188             PIC 9(04) VALUE 9316.
               10  SCB-DESC-004188          PIC X(18) VALUE 'DESC-054444'.
           05  SCB-GRP-0382.
               10  SCB-E-004189             OCCURS 3 TIMES.
                   15  SCB-EK-004189        PIC X(10).
                   15  SCB-EV-004189        PIC 9(06) COMP.
               10  SCB-A-004190             PIC 9(09) COMP VALUE 4190.
               10  SCB-B-004191             PIC S9(07)V99 COMP-3 VALUE +0004191.
               10  SCB-C-004192             PIC X(24) VALUE 'FIELD-004192-ALPHA'.
               10  SCB-D-004193             PIC 9(04) VALUE 9351.
               10  SCB-DESC-004193          PIC X(18) VALUE 'DESC-054509'.
               10  SCB-E-004194             OCCURS 4 TIMES.
                   15  SCB-EK-004194        PIC X(10).
                   15  SCB-EV-004194        PIC 9(06) COMP.
               10  SCB-A-004195             PIC 9(09) COMP VALUE 4195.
               10  SCB-B-004196             PIC S9(07)V99 COMP-3 VALUE +0004196.
               10  SCB-C-004197             PIC X(24) VALUE 'FIELD-004197-ALPHA'.
               10  SCB-D-004198             PIC 9(04) VALUE 9386.
               10  SCB-DESC-004198          PIC X(18) VALUE 'DESC-054574'.
               10  SCB-E-004199             OCCURS 5 TIMES.
                   15  SCB-EK-004199        PIC X(10).
                   15  SCB-EV-004199        PIC 9(06) COMP.
               10  SCB-A-004200             PIC 9(09) COMP VALUE 4200.
           05  SCB-GRP-0383.
               10  SCB-B-004201             PIC S9(07)V99 COMP-3 VALUE +0004201.
               10  SCB-C-004202             PIC X(24) VALUE 'FIELD-004202-ALPHA'.
               10  SCB-D-004203             PIC 9(04) VALUE 9421.
               10  SCB-DESC-004203          PIC X(18) VALUE 'DESC-054639'.
               10  SCB-E-004204             OCCURS 2 TIMES.
                   15  SCB-EK-004204        PIC X(10).
                   15  SCB-EV-004204        PIC 9(06) COMP.
               10  SCB-A-004205             PIC 9(09) COMP VALUE 4205.
               10  SCB-B-004206             PIC S9(07)V99 COMP-3 VALUE +0004206.
               10  SCB-C-004207             PIC X(24) VALUE 'FIELD-004207-ALPHA'.
               10  SCB-D-004208             PIC 9(04) VALUE 9456.
               10  SCB-DESC-004208          PIC X(18) VALUE 'DESC-054704'.
               10  SCB-E-004209             OCCURS 3 TIMES.
                   15  SCB-EK-004209        PIC X(10).
                   15  SCB-EV-004209        PIC 9(06) COMP.
               10  SCB-A-004210             PIC 9(09) COMP VALUE 4210.
               10  SCB-B-004211             PIC S9(07)V99 COMP-3 VALUE +0004211.
               10  SCB-C-004212             PIC X(24) VALUE 'FIELD-004212-ALPHA'.
               10  SCB-D-004213             PIC 9(04) VALUE 9491.
               10  SCB-DESC-004213          PIC X(18) VALUE 'DESC-054769'.
           05  SCB-GRP-0384.
               10  SCB-E-004214             OCCURS 4 TIMES.
                   15  SCB-EK-004214        PIC X(10).
                   15  SCB-EV-004214        PIC 9(06) COMP.
               10  SCB-A-004215             PIC 9(09) COMP VALUE 4215.
               10  SCB-B-004216             PIC S9(07)V99 COMP-3 VALUE +0004216.
               10  SCB-C-004217             PIC X(24) VALUE 'FIELD-004217-ALPHA'.
               10  SCB-D-004218             PIC 9(04) VALUE 9526.
               10  SCB-DESC-004218          PIC X(18) VALUE 'DESC-054834'.
               10  SCB-E-004219             OCCURS 5 TIMES.
                   15  SCB-EK-004219        PIC X(10).
                   15  SCB-EV-004219        PIC 9(06) COMP.
               10  SCB-A-004220             PIC 9(09) COMP VALUE 4220.
               10  SCB-B-004221             PIC S9(07)V99 COMP-3 VALUE +0004221.
               10  SCB-C-004222             PIC X(24) VALUE 'FIELD-004222-ALPHA'.
               10  SCB-D-004223             PIC 9(04) VALUE 9561.
               10  SCB-DESC-004223          PIC X(18) VALUE 'DESC-054899'.
               10  SCB-E-004224             OCCURS 2 TIMES.
                   15  SCB-EK-004224        PIC X(10).
                   15  SCB-EV-004224        PIC 9(06) COMP.
               10  SCB-A-004225             PIC 9(09) COMP VALUE 4225.
               10  SCB-B-004226             PIC S9(07)V99 COMP-3 VALUE +0004226.
               10  SCB-C-004227             PIC X(24) VALUE 'FIELD-004227-ALPHA'.
           05  SCB-ALT-0064 REDEFINES SCB-GRP-0384.
               10  SCB-ALT-FLAG-0064      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0064   PIC X(64).
           05  SCB-GRP-0385.
               10  SCB-D-004228             PIC 9(04) VALUE 9596.
               10  SCB-DESC-004228          PIC X(18) VALUE 'DESC-054964'.
               10  SCB-E-004229             OCCURS 3 TIMES.
                   15  SCB-EK-004229        PIC X(10).
                   15  SCB-EV-004229        PIC 9(06) COMP.
               10  SCB-A-004230             PIC 9(09) COMP VALUE 4230.
               10  SCB-B-004231             PIC S9(07)V99 COMP-3 VALUE +0004231.
               10  SCB-C-004232             PIC X(24) VALUE 'FIELD-004232-ALPHA'.
               10  SCB-D-004233             PIC 9(04) VALUE 9631.
               10  SCB-DESC-004233          PIC X(18) VALUE 'DESC-055029'.
               10  SCB-E-004234             OCCURS 4 TIMES.
                   15  SCB-EK-004234        PIC X(10).
                   15  SCB-EV-004234        PIC 9(06) COMP.
               10  SCB-A-004235             PIC 9(09) COMP VALUE 4235.
           05  SCB-GRP-0386.
               10  SCB-B-004236             PIC S9(07)V99 COMP-3 VALUE +0004236.
               10  SCB-C-004237             PIC X(24) VALUE 'FIELD-004237-ALPHA'.
               10  SCB-D-004238             PIC 9(04) VALUE 9666.
               10  SCB-DESC-004238          PIC X(18) VALUE 'DESC-055094'.
               10  SCB-E-004239             OCCURS 5 TIMES.
                   15  SCB-EK-004239        PIC X(10).
                   15  SCB-EV-004239        PIC 9(06) COMP.
               10  SCB-A-004240             PIC 9(09) COMP VALUE 4240.
               10  SCB-B-004241             PIC S9(07)V99 COMP-3 VALUE +0004241.
               10  SCB-C-004242             PIC X(24) VALUE 'FIELD-004242-ALPHA'.
               10  SCB-D-004243             PIC 9(04) VALUE 9701.
               10  SCB-DESC-004243          PIC X(18) VALUE 'DESC-055159'.
               10  SCB-E-004244             OCCURS 2 TIMES.
                   15  SCB-EK-004244        PIC X(10).
                   15  SCB-EV-004244        PIC 9(06) COMP.
           05  SCB-GRP-0387.
               10  SCB-A-004245             PIC 9(09) COMP VALUE 4245.
               10  SCB-B-004246             PIC S9(07)V99 COMP-3 VALUE +0004246.
               10  SCB-C-004247             PIC X(24) VALUE 'FIELD-004247-ALPHA'.
               10  SCB-D-004248             PIC 9(04) VALUE 9736.
               10  SCB-DESC-004248          PIC X(18) VALUE 'DESC-055224'.
               10  SCB-E-004249             OCCURS 3 TIMES.
                   15  SCB-EK-004249        PIC X(10).
                   15  SCB-EV-004249        PIC 9(06) COMP.
               10  SCB-A-004250             PIC 9(09) COMP VALUE 4250.
               10  SCB-B-004251             PIC S9(07)V99 COMP-3 VALUE +0004251.
               10  SCB-C-004252             PIC X(24) VALUE 'FIELD-004252-ALPHA'.
               10  SCB-D-004253             PIC 9(04) VALUE 9771.
               10  SCB-DESC-004253          PIC X(18) VALUE 'DESC-055289'.
               10  SCB-E-004254             OCCURS 4 TIMES.
                   15  SCB-EK-004254        PIC X(10).
                   15  SCB-EV-004254        PIC 9(06) COMP.
           05  SCB-GRP-0388.
               10  SCB-A-004255             PIC 9(09) COMP VALUE 4255.
               10  SCB-B-004256             PIC S9(07)V99 COMP-3 VALUE +0004256.
               10  SCB-C-004257             PIC X(24) VALUE 'FIELD-004257-ALPHA'.
               10  SCB-D-004258             PIC 9(04) VALUE 9806.
               10  SCB-DESC-004258          PIC X(18) VALUE 'DESC-055354'.
               10  SCB-E-004259             OCCURS 5 TIMES.
                   15  SCB-EK-004259        PIC X(10).
                   15  SCB-EV-004259        PIC 9(06) COMP.
               10  SCB-A-004260             PIC 9(09) COMP VALUE 4260.
               10  SCB-B-004261             PIC S9(07)V99 COMP-3 VALUE +0004261.
               10  SCB-C-004262             PIC X(24) VALUE 'FIELD-004262-ALPHA'.
               10  SCB-D-004263             PIC 9(04) VALUE 9841.
               10  SCB-DESC-004263          PIC X(18) VALUE 'DESC-055419'.
               10  SCB-E-004264             OCCURS 2 TIMES.
                   15  SCB-EK-004264        PIC X(10).
                   15  SCB-EV-004264        PIC 9(06) COMP.
               10  SCB-A-004265             PIC 9(09) COMP VALUE 4265.
           05  SCB-GRP-0389.
               10  SCB-B-004266             PIC S9(07)V99 COMP-3 VALUE +0004266.
               10  SCB-C-004267             PIC X(24) VALUE 'FIELD-004267-ALPHA'.
               10  SCB-D-004268             PIC 9(04) VALUE 9876.
               10  SCB-DESC-004268          PIC X(18) VALUE 'DESC-055484'.
               10  SCB-E-004269             OCCURS 3 TIMES.
                   15  SCB-EK-004269        PIC X(10).
                   15  SCB-EV-004269        PIC 9(06) COMP.
               10  SCB-A-004270             PIC 9(09) COMP VALUE 4270.
               10  SCB-B-004271             PIC S9(07)V99 COMP-3 VALUE +0004271.
               10  SCB-C-004272             PIC X(24) VALUE 'FIELD-004272-ALPHA'.
               10  SCB-D-004273             PIC 9(04) VALUE 9911.
               10  SCB-DESC-004273          PIC X(18) VALUE 'DESC-055549'.
               10  SCB-E-004274             OCCURS 4 TIMES.
                   15  SCB-EK-004274        PIC X(10).
                   15  SCB-EV-004274        PIC 9(06) COMP.
               10  SCB-A-004275             PIC 9(09) COMP VALUE 4275.
               10  SCB-B-004276             PIC S9(07)V99 COMP-3 VALUE +0004276.
               10  SCB-C-004277             PIC X(24) VALUE 'FIELD-004277-ALPHA'.
           05  SCB-GRP-0390.
               10  SCB-D-004278             PIC 9(04) VALUE 9946.
               10  SCB-DESC-004278          PIC X(18) VALUE 'DESC-055614'.
               10  SCB-E-004279             OCCURS 5 TIMES.
                   15  SCB-EK-004279        PIC X(10).
                   15  SCB-EV-004279        PIC 9(06) COMP.
               10  SCB-A-004280             PIC 9(09) COMP VALUE 4280.
               10  SCB-B-004281             PIC S9(07)V99 COMP-3 VALUE +0004281.
               10  SCB-C-004282             PIC X(24) VALUE 'FIELD-004282-ALPHA'.
               10  SCB-D-004283             PIC 9(04) VALUE 9981.
               10  SCB-DESC-004283          PIC X(18) VALUE 'DESC-055679'.
               10  SCB-E-004284             OCCURS 2 TIMES.
                   15  SCB-EK-004284        PIC X(10).
                   15  SCB-EV-004284        PIC 9(06) COMP.
               10  SCB-A-004285             PIC 9(09) COMP VALUE 4285.
               10  SCB-B-004286             PIC S9(07)V99 COMP-3 VALUE +0004286.
               10  SCB-C-004287             PIC X(24) VALUE 'FIELD-004287-ALPHA'.
               10  SCB-D-004288             PIC 9(04) VALUE 16.
               10  SCB-DESC-004288          PIC X(18) VALUE 'DESC-055744'.
               10  SCB-E-004289             OCCURS 3 TIMES.
                   15  SCB-EK-004289        PIC X(10).
                   15  SCB-EV-004289        PIC 9(06) COMP.
               10  SCB-A-004290             PIC 9(09) COMP VALUE 4290.
           05  SCB-ALT-0065 REDEFINES SCB-GRP-0390.
               10  SCB-ALT-FLAG-0065      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0065   PIC X(64).
           05  SCB-GRP-0391.
               10  SCB-B-004291             PIC S9(07)V99 COMP-3 VALUE +0004291.
               10  SCB-C-004292             PIC X(24) VALUE 'FIELD-004292-ALPHA'.
               10  SCB-D-004293             PIC 9(04) VALUE 51.
               10  SCB-DESC-004293          PIC X(18) VALUE 'DESC-055809'.
               10  SCB-E-004294             OCCURS 4 TIMES.
                   15  SCB-EK-004294        PIC X(10).
                   15  SCB-EV-004294        PIC 9(06) COMP.
               10  SCB-A-004295             PIC 9(09) COMP VALUE 4295.
               10  SCB-B-004296             PIC S9(07)V99 COMP-3 VALUE +0004296.
               10  SCB-C-004297             PIC X(24) VALUE 'FIELD-004297-ALPHA'.
               10  SCB-D-004298             PIC 9(04) VALUE 86.
               10  SCB-DESC-004298          PIC X(18) VALUE 'DESC-055874'.
               10  SCB-E-004299             OCCURS 5 TIMES.
                   15  SCB-EK-004299        PIC X(10).
                   15  SCB-EV-004299        PIC 9(06) COMP.
               10  SCB-A-004300             PIC 9(09) COMP VALUE 4300.
               10  SCB-B-004301             PIC S9(07)V99 COMP-3 VALUE +0004301.
               10  SCB-C-004302             PIC X(24) VALUE 'FIELD-004302-ALPHA'.
               10  SCB-D-004303             PIC 9(04) VALUE 121.
               10  SCB-DESC-004303          PIC X(18) VALUE 'DESC-055939'.
               10  SCB-E-004304             OCCURS 2 TIMES.
                   15  SCB-EK-004304        PIC X(10).
                   15  SCB-EV-004304        PIC 9(06) COMP.
           05  SCB-GRP-0392.
               10  SCB-A-004305             PIC 9(09) COMP VALUE 4305.
               10  SCB-B-004306             PIC S9(07)V99 COMP-3 VALUE +0004306.
               10  SCB-C-004307             PIC X(24) VALUE 'FIELD-004307-ALPHA'.
               10  SCB-D-004308             PIC 9(04) VALUE 156.
               10  SCB-DESC-004308          PIC X(18) VALUE 'DESC-056004'.
               10  SCB-E-004309             OCCURS 3 TIMES.
                   15  SCB-EK-004309        PIC X(10).
                   15  SCB-EV-004309        PIC 9(06) COMP.
               10  SCB-A-004310             PIC 9(09) COMP VALUE 4310.
               10  SCB-B-004311             PIC S9(07)V99 COMP-3 VALUE +0004311.
               10  SCB-C-004312             PIC X(24) VALUE 'FIELD-004312-ALPHA'.
           05  SCB-GRP-0393.
               10  SCB-D-004313             PIC 9(04) VALUE 191.
               10  SCB-DESC-004313          PIC X(18) VALUE 'DESC-056069'.
               10  SCB-E-004314             OCCURS 4 TIMES.
                   15  SCB-EK-004314        PIC X(10).
                   15  SCB-EV-004314        PIC 9(06) COMP.
               10  SCB-A-004315             PIC 9(09) COMP VALUE 4315.
               10  SCB-B-004316             PIC S9(07)V99 COMP-3 VALUE +0004316.
               10  SCB-C-004317             PIC X(24) VALUE 'FIELD-004317-ALPHA'.
               10  SCB-D-004318             PIC 9(04) VALUE 226.
               10  SCB-DESC-004318          PIC X(18) VALUE 'DESC-056134'.
               10  SCB-E-004319             OCCURS 5 TIMES.
                   15  SCB-EK-004319        PIC X(10).
                   15  SCB-EV-004319        PIC 9(06) COMP.
               10  SCB-A-004320             PIC 9(09) COMP VALUE 4320.
               10  SCB-B-004321             PIC S9(07)V99 COMP-3 VALUE +0004321.
           05  SCB-GRP-0394.
               10  SCB-C-004322             PIC X(24) VALUE 'FIELD-004322-ALPHA'.
               10  SCB-D-004323             PIC 9(04) VALUE 261.
               10  SCB-DESC-004323          PIC X(18) VALUE 'DESC-056199'.
               10  SCB-E-004324             OCCURS 2 TIMES.
                   15  SCB-EK-004324        PIC X(10).
                   15  SCB-EV-004324        PIC 9(06) COMP.
               10  SCB-A-004325             PIC 9(09) COMP VALUE 4325.
               10  SCB-B-004326             PIC S9(07)V99 COMP-3 VALUE +0004326.
               10  SCB-C-004327             PIC X(24) VALUE 'FIELD-004327-ALPHA'.
               10  SCB-D-004328             PIC 9(04) VALUE 296.
               10  SCB-DESC-004328          PIC X(18) VALUE 'DESC-056264'.
               10  SCB-E-004329             OCCURS 3 TIMES.
                   15  SCB-EK-004329        PIC X(10).
                   15  SCB-EV-004329        PIC 9(06) COMP.
               10  SCB-A-004330             PIC 9(09) COMP VALUE 4330.
               10  SCB-B-004331             PIC S9(07)V99 COMP-3 VALUE +0004331.
           05  SCB-GRP-0395.
               10  SCB-C-004332             PIC X(24) VALUE 'FIELD-004332-ALPHA'.
               10  SCB-D-004333             PIC 9(04) VALUE 331.
               10  SCB-DESC-004333          PIC X(18) VALUE 'DESC-056329'.
               10  SCB-E-004334             OCCURS 4 TIMES.
                   15  SCB-EK-004334        PIC X(10).
                   15  SCB-EV-004334        PIC 9(06) COMP.
               10  SCB-A-004335             PIC 9(09) COMP VALUE 4335.
               10  SCB-B-004336             PIC S9(07)V99 COMP-3 VALUE +0004336.
               10  SCB-C-004337             PIC X(24) VALUE 'FIELD-004337-ALPHA'.
               10  SCB-D-004338             PIC 9(04) VALUE 366.
               10  SCB-DESC-004338          PIC X(18) VALUE 'DESC-056394'.
               10  SCB-E-004339             OCCURS 5 TIMES.
                   15  SCB-EK-004339        PIC X(10).
                   15  SCB-EV-004339        PIC 9(06) COMP.
               10  SCB-A-004340             PIC 9(09) COMP VALUE 4340.
               10  SCB-B-004341             PIC S9(07)V99 COMP-3 VALUE +0004341.
               10  SCB-C-004342             PIC X(24) VALUE 'FIELD-004342-ALPHA'.
           05  SCB-GRP-0396.
               10  SCB-D-004343             PIC 9(04) VALUE 401.
               10  SCB-DESC-004343          PIC X(18) VALUE 'DESC-056459'.
               10  SCB-E-004344             OCCURS 2 TIMES.
                   15  SCB-EK-004344        PIC X(10).
                   15  SCB-EV-004344        PIC 9(06) COMP.
               10  SCB-A-004345             PIC 9(09) COMP VALUE 4345.
               10  SCB-B-004346             PIC S9(07)V99 COMP-3 VALUE +0004346.
               10  SCB-C-004347             PIC X(24) VALUE 'FIELD-004347-ALPHA'.
               10  SCB-D-004348             PIC 9(04) VALUE 436.
               10  SCB-DESC-004348          PIC X(18) VALUE 'DESC-056524'.
               10  SCB-E-004349             OCCURS 3 TIMES.
                   15  SCB-EK-004349        PIC X(10).
                   15  SCB-EV-004349        PIC 9(06) COMP.
               10  SCB-A-004350             PIC 9(09) COMP VALUE 4350.
               10  SCB-B-004351             PIC S9(07)V99 COMP-3 VALUE +0004351.
               10  SCB-C-004352             PIC X(24) VALUE 'FIELD-004352-ALPHA'.
               10  SCB-D-004353             PIC 9(04) VALUE 471.
               10  SCB-DESC-004353          PIC X(18) VALUE 'DESC-056589'.
               10  SCB-E-004354             OCCURS 4 TIMES.
                   15  SCB-EK-004354        PIC X(10).
                   15  SCB-EV-004354        PIC 9(06) COMP.
           05  SCB-ALT-0066 REDEFINES SCB-GRP-0396.
               10  SCB-ALT-FLAG-0066      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0066   PIC X(64).
           05  SCB-GRP-0397.
               10  SCB-A-004355             PIC 9(09) COMP VALUE 4355.
               10  SCB-B-004356             PIC S9(07)V99 COMP-3 VALUE +0004356.
               10  SCB-C-004357             PIC X(24) VALUE 'FIELD-004357-ALPHA'.
               10  SCB-D-004358             PIC 9(04) VALUE 506.
               10  SCB-DESC-004358          PIC X(18) VALUE 'DESC-056654'.
               10  SCB-E-004359             OCCURS 5 TIMES.
                   15  SCB-EK-004359        PIC X(10).
                   15  SCB-EV-004359        PIC 9(06) COMP.
               10  SCB-A-004360             PIC 9(09) COMP VALUE 4360.
               10  SCB-B-004361             PIC S9(07)V99 COMP-3 VALUE +0004361.
               10  SCB-C-004362             PIC X(24) VALUE 'FIELD-004362-ALPHA'.
               10  SCB-D-004363             PIC 9(04) VALUE 541.
               10  SCB-DESC-004363          PIC X(18) VALUE 'DESC-056719'.
               10  SCB-E-004364             OCCURS 2 TIMES.
                   15  SCB-EK-004364        PIC X(10).
                   15  SCB-EV-004364        PIC 9(06) COMP.
               10  SCB-A-004365             PIC 9(09) COMP VALUE 4365.
               10  SCB-B-004366             PIC S9(07)V99 COMP-3 VALUE +0004366.
               10  SCB-C-004367             PIC X(24) VALUE 'FIELD-004367-ALPHA'.
           05  SCB-GRP-0398.
               10  SCB-D-004368             PIC 9(04) VALUE 576.
               10  SCB-DESC-004368          PIC X(18) VALUE 'DESC-056784'.
               10  SCB-E-004369             OCCURS 3 TIMES.
                   15  SCB-EK-004369        PIC X(10).
                   15  SCB-EV-004369        PIC 9(06) COMP.
               10  SCB-A-004370             PIC 9(09) COMP VALUE 4370.
               10  SCB-B-004371             PIC S9(07)V99 COMP-3 VALUE +0004371.
               10  SCB-C-004372             PIC X(24) VALUE 'FIELD-004372-ALPHA'.
               10  SCB-D-004373             PIC 9(04) VALUE 611.
               10  SCB-DESC-004373          PIC X(18) VALUE 'DESC-056849'.
               10  SCB-E-004374             OCCURS 4 TIMES.
                   15  SCB-EK-004374        PIC X(10).
                   15  SCB-EV-004374        PIC 9(06) COMP.
               10  SCB-A-004375             PIC 9(09) COMP VALUE 4375.
               10  SCB-B-004376             PIC S9(07)V99 COMP-3 VALUE +0004376.
               10  SCB-C-004377             PIC X(24) VALUE 'FIELD-004377-ALPHA'.
               10  SCB-D-004378             PIC 9(04) VALUE 646.
               10  SCB-DESC-004378          PIC X(18) VALUE 'DESC-056914'.
               10  SCB-E-004379             OCCURS 5 TIMES.
                   15  SCB-EK-004379        PIC X(10).
                   15  SCB-EV-004379        PIC 9(06) COMP.
               10  SCB-A-004380             PIC 9(09) COMP VALUE 4380.
               10  SCB-B-004381             PIC S9(07)V99 COMP-3 VALUE +0004381.
           05  SCB-GRP-0399.
               10  SCB-C-004382             PIC X(24) VALUE 'FIELD-004382-ALPHA'.
               10  SCB-D-004383             PIC 9(04) VALUE 681.
               10  SCB-DESC-004383          PIC X(18) VALUE 'DESC-056979'.
               10  SCB-E-004384             OCCURS 2 TIMES.
                   15  SCB-EK-004384        PIC X(10).
                   15  SCB-EV-004384        PIC 9(06) COMP.
               10  SCB-A-004385             PIC 9(09) COMP VALUE 4385.
               10  SCB-B-004386             PIC S9(07)V99 COMP-3 VALUE +0004386.
               10  SCB-C-004387             PIC X(24) VALUE 'FIELD-004387-ALPHA'.
               10  SCB-D-004388             PIC 9(04) VALUE 716.
               10  SCB-DESC-004388          PIC X(18) VALUE 'DESC-057044'.
               10  SCB-E-004389             OCCURS 3 TIMES.
                   15  SCB-EK-004389        PIC X(10).
                   15  SCB-EV-004389        PIC 9(06) COMP.
           05  SCB-GRP-0400.
               10  SCB-A-004390             PIC 9(09) COMP VALUE 4390.
               10  SCB-B-004391             PIC S9(07)V99 COMP-3 VALUE +0004391.
               10  SCB-C-004392             PIC X(24) VALUE 'FIELD-004392-ALPHA'.
               10  SCB-D-004393             PIC 9(04) VALUE 751.
               10  SCB-DESC-004393          PIC X(18) VALUE 'DESC-057109'.
               10  SCB-E-004394             OCCURS 4 TIMES.
                   15  SCB-EK-004394        PIC X(10).
                   15  SCB-EV-004394        PIC 9(06) COMP.
               10  SCB-A-004395             PIC 9(09) COMP VALUE 4395.
               10  SCB-B-004396             PIC S9(07)V99 COMP-3 VALUE +0004396.
               10  SCB-C-004397             PIC X(24) VALUE 'FIELD-004397-ALPHA'.
               10  SCB-D-004398             PIC 9(04) VALUE 786.
               10  SCB-DESC-004398          PIC X(18) VALUE 'DESC-057174'.
           05  SCB-GRP-0401.
               10  SCB-E-004399             OCCURS 5 TIMES.
                   15  SCB-EK-004399        PIC X(10).
                   15  SCB-EV-004399        PIC 9(06) COMP.
               10  SCB-A-004400             PIC 9(09) COMP VALUE 4400.
               10  SCB-B-004401             PIC S9(07)V99 COMP-3 VALUE +0004401.
               10  SCB-C-004402             PIC X(24) VALUE 'FIELD-004402-ALPHA'.
               10  SCB-D-004403             PIC 9(04) VALUE 821.
               10  SCB-DESC-004403          PIC X(18) VALUE 'DESC-057239'.
               10  SCB-E-004404             OCCURS 2 TIMES.
                   15  SCB-EK-004404        PIC X(10).
                   15  SCB-EV-004404        PIC 9(06) COMP.
               10  SCB-A-004405             PIC 9(09) COMP VALUE 4405.
               10  SCB-B-004406             PIC S9(07)V99 COMP-3 VALUE +0004406.
               10  SCB-C-004407             PIC X(24) VALUE 'FIELD-004407-ALPHA'.
               10  SCB-D-004408             PIC 9(04) VALUE 856.
               10  SCB-DESC-004408          PIC X(18) VALUE 'DESC-057304'.
           05  SCB-GRP-0402.
               10  SCB-E-004409             OCCURS 3 TIMES.
                   15  SCB-EK-004409        PIC X(10).
                   15  SCB-EV-004409        PIC 9(06) COMP.
               10  SCB-A-004410             PIC 9(09) COMP VALUE 4410.
               10  SCB-B-004411             PIC S9(07)V99 COMP-3 VALUE +0004411.
               10  SCB-C-004412             PIC X(24) VALUE 'FIELD-004412-ALPHA'.
               10  SCB-D-004413             PIC 9(04) VALUE 891.
               10  SCB-DESC-004413          PIC X(18) VALUE 'DESC-057369'.
               10  SCB-E-004414             OCCURS 4 TIMES.
                   15  SCB-EK-004414        PIC X(10).
                   15  SCB-EV-004414        PIC 9(06) COMP.
               10  SCB-A-004415             PIC 9(09) COMP VALUE 4415.
               10  SCB-B-004416             PIC S9(07)V99 COMP-3 VALUE +0004416.
               10  SCB-C-004417             PIC X(24) VALUE 'FIELD-004417-ALPHA'.
               10  SCB-D-004418             PIC 9(04) VALUE 926.
               10  SCB-DESC-004418          PIC X(18) VALUE 'DESC-057434'.
               10  SCB-E-004419             OCCURS 5 TIMES.
                   15  SCB-EK-004419        PIC X(10).
                   15  SCB-EV-004419        PIC 9(06) COMP.
           05  SCB-ALT-0067 REDEFINES SCB-GRP-0402.
               10  SCB-ALT-FLAG-0067      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0067   PIC X(64).
           05  SCB-GRP-0403.
               10  SCB-A-004420             PIC 9(09) COMP VALUE 4420.
               10  SCB-B-004421             PIC S9(07)V99 COMP-3 VALUE +0004421.
               10  SCB-C-004422             PIC X(24) VALUE 'FIELD-004422-ALPHA'.
               10  SCB-D-004423             PIC 9(04) VALUE 961.
               10  SCB-DESC-004423          PIC X(18) VALUE 'DESC-057499'.
               10  SCB-E-004424             OCCURS 2 TIMES.
                   15  SCB-EK-004424        PIC X(10).
                   15  SCB-EV-004424        PIC 9(06) COMP.
               10  SCB-A-004425             PIC 9(09) COMP VALUE 4425.
               10  SCB-B-004426             PIC S9(07)V99 COMP-3 VALUE +0004426.
               10  SCB-C-004427             PIC X(24) VALUE 'FIELD-004427-ALPHA'.
               10  SCB-D-004428             PIC 9(04) VALUE 996.
               10  SCB-DESC-004428          PIC X(18) VALUE 'DESC-057564'.
               10  SCB-E-004429             OCCURS 3 TIMES.
                   15  SCB-EK-004429        PIC X(10).
                   15  SCB-EV-004429        PIC 9(06) COMP.
               10  SCB-A-004430             PIC 9(09) COMP VALUE 4430.
               10  SCB-B-004431             PIC S9(07)V99 COMP-3 VALUE +0004431.
           05  SCB-GRP-0404.
               10  SCB-C-004432             PIC X(24) VALUE 'FIELD-004432-ALPHA'.
               10  SCB-D-004433             PIC 9(04) VALUE 1031.
               10  SCB-DESC-004433          PIC X(18) VALUE 'DESC-057629'.
               10  SCB-E-004434             OCCURS 4 TIMES.
                   15  SCB-EK-004434        PIC X(10).
                   15  SCB-EV-004434        PIC 9(06) COMP.
               10  SCB-A-004435             PIC 9(09) COMP VALUE 4435.
               10  SCB-B-004436             PIC S9(07)V99 COMP-3 VALUE +0004436.
               10  SCB-C-004437             PIC X(24) VALUE 'FIELD-004437-ALPHA'.
               10  SCB-D-004438             PIC 9(04) VALUE 1066.
               10  SCB-DESC-004438          PIC X(18) VALUE 'DESC-057694'.
               10  SCB-E-004439             OCCURS 5 TIMES.
                   15  SCB-EK-004439        PIC X(10).
                   15  SCB-EV-004439        PIC 9(06) COMP.
               10  SCB-A-004440             PIC 9(09) COMP VALUE 4440.
               10  SCB-B-004441             PIC S9(07)V99 COMP-3 VALUE +0004441.
               10  SCB-C-004442             PIC X(24) VALUE 'FIELD-004442-ALPHA'.
               10  SCB-D-004443             PIC 9(04) VALUE 1101.
               10  SCB-DESC-004443          PIC X(18) VALUE 'DESC-057759'.
               10  SCB-E-004444             OCCURS 2 TIMES.
                   15  SCB-EK-004444        PIC X(10).
                   15  SCB-EV-004444        PIC 9(06) COMP.
           05  SCB-GRP-0405.
               10  SCB-A-004445             PIC 9(09) COMP VALUE 4445.
               10  SCB-B-004446             PIC S9(07)V99 COMP-3 VALUE +0004446.
               10  SCB-C-004447             PIC X(24) VALUE 'FIELD-004447-ALPHA'.
               10  SCB-D-004448             PIC 9(04) VALUE 1136.
               10  SCB-DESC-004448          PIC X(18) VALUE 'DESC-057824'.
               10  SCB-E-004449             OCCURS 3 TIMES.
                   15  SCB-EK-004449        PIC X(10).
                   15  SCB-EV-004449        PIC 9(06) COMP.
               10  SCB-A-004450             PIC 9(09) COMP VALUE 4450.
               10  SCB-B-004451             PIC S9(07)V99 COMP-3 VALUE +0004451.
               10  SCB-C-004452             PIC X(24) VALUE 'FIELD-004452-ALPHA'.
               10  SCB-D-004453             PIC 9(04) VALUE 1171.
               10  SCB-DESC-004453          PIC X(18) VALUE 'DESC-057889'.
               10  SCB-E-004454             OCCURS 4 TIMES.
                   15  SCB-EK-004454        PIC X(10).
                   15  SCB-EV-004454        PIC 9(06) COMP.
               10  SCB-A-004455             PIC 9(09) COMP VALUE 4455.
               10  SCB-B-004456             PIC S9(07)V99 COMP-3 VALUE +0004456.
               10  SCB-C-004457             PIC X(24) VALUE 'FIELD-004457-ALPHA'.
               10  SCB-D-004458             PIC 9(04) VALUE 1206.
               10  SCB-DESC-004458          PIC X(18) VALUE 'DESC-057954'.
           05  SCB-GRP-0406.
               10  SCB-E-004459             OCCURS 5 TIMES.
                   15  SCB-EK-004459        PIC X(10).
                   15  SCB-EV-004459        PIC 9(06) COMP.
               10  SCB-A-004460             PIC 9(09) COMP VALUE 4460.
               10  SCB-B-004461             PIC S9(07)V99 COMP-3 VALUE +0004461.
               10  SCB-C-004462             PIC X(24) VALUE 'FIELD-004462-ALPHA'.
               10  SCB-D-004463             PIC 9(04) VALUE 1241.
               10  SCB-DESC-004463          PIC X(18) VALUE 'DESC-058019'.
               10  SCB-E-004464             OCCURS 2 TIMES.
                   15  SCB-EK-004464        PIC X(10).
                   15  SCB-EV-004464        PIC 9(06) COMP.
               10  SCB-A-004465             PIC 9(09) COMP VALUE 4465.
               10  SCB-B-004466             PIC S9(07)V99 COMP-3 VALUE +0004466.
           05  SCB-GRP-0407.
               10  SCB-C-004467             PIC X(24) VALUE 'FIELD-004467-ALPHA'.
               10  SCB-D-004468             PIC 9(04) VALUE 1276.
               10  SCB-DESC-004468          PIC X(18) VALUE 'DESC-058084'.
               10  SCB-E-004469             OCCURS 3 TIMES.
                   15  SCB-EK-004469        PIC X(10).
                   15  SCB-EV-004469        PIC 9(06) COMP.
               10  SCB-A-004470             PIC 9(09) COMP VALUE 4470.
               10  SCB-B-004471             PIC S9(07)V99 COMP-3 VALUE +0004471.
               10  SCB-C-004472             PIC X(24) VALUE 'FIELD-004472-ALPHA'.
               10  SCB-D-004473             PIC 9(04) VALUE 1311.
               10  SCB-DESC-004473          PIC X(18) VALUE 'DESC-058149'.
               10  SCB-E-004474             OCCURS 4 TIMES.
                   15  SCB-EK-004474        PIC X(10).
                   15  SCB-EV-004474        PIC 9(06) COMP.
               10  SCB-A-004475             PIC 9(09) COMP VALUE 4475.
           05  SCB-GRP-0408.
               10  SCB-B-004476             PIC S9(07)V99 COMP-3 VALUE +0004476.
               10  SCB-C-004477             PIC X(24) VALUE 'FIELD-004477-ALPHA'.
               10  SCB-D-004478             PIC 9(04) VALUE 1346.
               10  SCB-DESC-004478          PIC X(18) VALUE 'DESC-058214'.
               10  SCB-E-004479             OCCURS 5 TIMES.
                   15  SCB-EK-004479        PIC X(10).
                   15  SCB-EV-004479        PIC 9(06) COMP.
               10  SCB-A-004480             PIC 9(09) COMP VALUE 4480.
               10  SCB-B-004481             PIC S9(07)V99 COMP-3 VALUE +0004481.
               10  SCB-C-004482             PIC X(24) VALUE 'FIELD-004482-ALPHA'.
               10  SCB-D-004483             PIC 9(04) VALUE 1381.
               10  SCB-DESC-004483          PIC X(18) VALUE 'DESC-058279'.
               10  SCB-E-004484             OCCURS 2 TIMES.
                   15  SCB-EK-004484        PIC X(10).
                   15  SCB-EV-004484        PIC 9(06) COMP.
               10  SCB-A-004485             PIC 9(09) COMP VALUE 4485.
           05  SCB-ALT-0068 REDEFINES SCB-GRP-0408.
               10  SCB-ALT-FLAG-0068      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0068   PIC X(64).
           05  SCB-GRP-0409.
               10  SCB-B-004486             PIC S9(07)V99 COMP-3 VALUE +0004486.
               10  SCB-C-004487             PIC X(24) VALUE 'FIELD-004487-ALPHA'.
               10  SCB-D-004488             PIC 9(04) VALUE 1416.
               10  SCB-DESC-004488          PIC X(18) VALUE 'DESC-058344'.
               10  SCB-E-004489             OCCURS 3 TIMES.
                   15  SCB-EK-004489        PIC X(10).
                   15  SCB-EV-004489        PIC 9(06) COMP.
               10  SCB-A-004490             PIC 9(09) COMP VALUE 4490.
               10  SCB-B-004491             PIC S9(07)V99 COMP-3 VALUE +0004491.
               10  SCB-C-004492             PIC X(24) VALUE 'FIELD-004492-ALPHA'.
               10  SCB-D-004493             PIC 9(04) VALUE 1451.
               10  SCB-DESC-004493          PIC X(18) VALUE 'DESC-058409'.
               10  SCB-E-004494             OCCURS 4 TIMES.
                   15  SCB-EK-004494        PIC X(10).
                   15  SCB-EV-004494        PIC 9(06) COMP.
               10  SCB-A-004495             PIC 9(09) COMP VALUE 4495.
               10  SCB-B-004496             PIC S9(07)V99 COMP-3 VALUE +0004496.
           05  SCB-GRP-0410.
               10  SCB-C-004497             PIC X(24) VALUE 'FIELD-004497-ALPHA'.
               10  SCB-D-004498             PIC 9(04) VALUE 1486.
               10  SCB-DESC-004498          PIC X(18) VALUE 'DESC-058474'.
               10  SCB-E-004499             OCCURS 5 TIMES.
                   15  SCB-EK-004499        PIC X(10).
                   15  SCB-EV-004499        PIC 9(06) COMP.
               10  SCB-A-004500             PIC 9(09) COMP VALUE 4500.
               10  SCB-B-004501             PIC S9(07)V99 COMP-3 VALUE +0004501.
               10  SCB-C-004502             PIC X(24) VALUE 'FIELD-004502-ALPHA'.
               10  SCB-D-004503             PIC 9(04) VALUE 1521.
               10  SCB-DESC-004503          PIC X(18) VALUE 'DESC-058539'.
               10  SCB-E-004504             OCCURS 2 TIMES.
                   15  SCB-EK-004504        PIC X(10).
                   15  SCB-EV-004504        PIC 9(06) COMP.
               10  SCB-A-004505             PIC 9(09) COMP VALUE 4505.
               10  SCB-B-004506             PIC S9(07)V99 COMP-3 VALUE +0004506.
               10  SCB-C-004507             PIC X(24) VALUE 'FIELD-004507-ALPHA'.
               10  SCB-D-004508             PIC 9(04) VALUE 1556.
               10  SCB-DESC-004508          PIC X(18) VALUE 'DESC-058604'.
           05  SCB-GRP-0411.
               10  SCB-E-004509             OCCURS 3 TIMES.
                   15  SCB-EK-004509        PIC X(10).
                   15  SCB-EV-004509        PIC 9(06) COMP.
               10  SCB-A-004510             PIC 9(09) COMP VALUE 4510.
               10  SCB-B-004511             PIC S9(07)V99 COMP-3 VALUE +0004511.
               10  SCB-C-004512             PIC X(24) VALUE 'FIELD-004512-ALPHA'.
               10  SCB-D-004513             PIC 9(04) VALUE 1591.
               10  SCB-DESC-004513          PIC X(18) VALUE 'DESC-058669'.
               10  SCB-E-004514             OCCURS 4 TIMES.
                   15  SCB-EK-004514        PIC X(10).
                   15  SCB-EV-004514        PIC 9(06) COMP.
               10  SCB-A-004515             PIC 9(09) COMP VALUE 4515.
               10  SCB-B-004516             PIC S9(07)V99 COMP-3 VALUE +0004516.
               10  SCB-C-004517             PIC X(24) VALUE 'FIELD-004517-ALPHA'.
               10  SCB-D-004518             PIC 9(04) VALUE 1626.
               10  SCB-DESC-004518          PIC X(18) VALUE 'DESC-058734'.
               10  SCB-E-004519             OCCURS 5 TIMES.
                   15  SCB-EK-004519        PIC X(10).
                   15  SCB-EV-004519        PIC 9(06) COMP.
               10  SCB-A-004520             PIC 9(09) COMP VALUE 4520.
               10  SCB-B-004521             PIC S9(07)V99 COMP-3 VALUE +0004521.
           05  SCB-GRP-0412.
               10  SCB-C-004522             PIC X(24) VALUE 'FIELD-004522-ALPHA'.
               10  SCB-D-004523             PIC 9(04) VALUE 1661.
               10  SCB-DESC-004523          PIC X(18) VALUE 'DESC-058799'.
               10  SCB-E-004524             OCCURS 2 TIMES.
                   15  SCB-EK-004524        PIC X(10).
                   15  SCB-EV-004524        PIC 9(06) COMP.
               10  SCB-A-004525             PIC 9(09) COMP VALUE 4525.
               10  SCB-B-004526             PIC S9(07)V99 COMP-3 VALUE +0004526.
               10  SCB-C-004527             PIC X(24) VALUE 'FIELD-004527-ALPHA'.
               10  SCB-D-004528             PIC 9(04) VALUE 1696.
               10  SCB-DESC-004528          PIC X(18) VALUE 'DESC-058864'.
               10  SCB-E-004529             OCCURS 3 TIMES.
                   15  SCB-EK-004529        PIC X(10).
                   15  SCB-EV-004529        PIC 9(06) COMP.
               10  SCB-A-004530             PIC 9(09) COMP VALUE 4530.
               10  SCB-B-004531             PIC S9(07)V99 COMP-3 VALUE +0004531.
               10  SCB-C-004532             PIC X(24) VALUE 'FIELD-004532-ALPHA'.
               10  SCB-D-004533             PIC 9(04) VALUE 1731.
               10  SCB-DESC-004533          PIC X(18) VALUE 'DESC-058929'.
               10  SCB-E-004534             OCCURS 4 TIMES.
                   15  SCB-EK-004534        PIC X(10).
                   15  SCB-EV-004534        PIC 9(06) COMP.
               10  SCB-A-004535             PIC 9(09) COMP VALUE 4535.
           05  SCB-GRP-0413.
               10  SCB-B-004536             PIC S9(07)V99 COMP-3 VALUE +0004536.
               10  SCB-C-004537             PIC X(24) VALUE 'FIELD-004537-ALPHA'.
               10  SCB-D-004538             PIC 9(04) VALUE 1766.
               10  SCB-DESC-004538          PIC X(18) VALUE 'DESC-058994'.
               10  SCB-E-004539             OCCURS 5 TIMES.
                   15  SCB-EK-004539        PIC X(10).
                   15  SCB-EV-004539        PIC 9(06) COMP.
               10  SCB-A-004540             PIC 9(09) COMP VALUE 4540.
               10  SCB-B-004541             PIC S9(07)V99 COMP-3 VALUE +0004541.
               10  SCB-C-004542             PIC X(24) VALUE 'FIELD-004542-ALPHA'.
               10  SCB-D-004543             PIC 9(04) VALUE 1801.
               10  SCB-DESC-004543          PIC X(18) VALUE 'DESC-059059'.
           05  SCB-GRP-0414.
               10  SCB-E-004544             OCCURS 2 TIMES.
                   15  SCB-EK-004544        PIC X(10).
                   15  SCB-EV-004544        PIC 9(06) COMP.
               10  SCB-A-004545             PIC 9(09) COMP VALUE 4545.
               10  SCB-B-004546             PIC S9(07)V99 COMP-3 VALUE +0004546.
               10  SCB-C-004547             PIC X(24) VALUE 'FIELD-004547-ALPHA'.
               10  SCB-D-004548             PIC 9(04) VALUE 1836.
               10  SCB-DESC-004548          PIC X(18) VALUE 'DESC-059124'.
               10  SCB-E-004549             OCCURS 3 TIMES.
                   15  SCB-EK-004549        PIC X(10).
                   15  SCB-EV-004549        PIC 9(06) COMP.
               10  SCB-A-004550             PIC 9(09) COMP VALUE 4550.
               10  SCB-B-004551             PIC S9(07)V99 COMP-3 VALUE +0004551.
               10  SCB-C-004552             PIC X(24) VALUE 'FIELD-004552-ALPHA'.
           05  SCB-ALT-0069 REDEFINES SCB-GRP-0414.
               10  SCB-ALT-FLAG-0069      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0069   PIC X(64).
           05  SCB-GRP-0415.
               10  SCB-D-004553             PIC 9(04) VALUE 1871.
               10  SCB-DESC-004553          PIC X(18) VALUE 'DESC-059189'.
               10  SCB-E-004554             OCCURS 4 TIMES.
                   15  SCB-EK-004554        PIC X(10).
                   15  SCB-EV-004554        PIC 9(06) COMP.
               10  SCB-A-004555             PIC 9(09) COMP VALUE 4555.
               10  SCB-B-004556             PIC S9(07)V99 COMP-3 VALUE +0004556.
               10  SCB-C-004557             PIC X(24) VALUE 'FIELD-004557-ALPHA'.
               10  SCB-D-004558             PIC 9(04) VALUE 1906.
               10  SCB-DESC-004558          PIC X(18) VALUE 'DESC-059254'.
               10  SCB-E-004559             OCCURS 5 TIMES.
                   15  SCB-EK-004559        PIC X(10).
                   15  SCB-EV-004559        PIC 9(06) COMP.
               10  SCB-A-004560             PIC 9(09) COMP VALUE 4560.
               10  SCB-B-004561             PIC S9(07)V99 COMP-3 VALUE +0004561.
               10  SCB-C-004562             PIC X(24) VALUE 'FIELD-004562-ALPHA'.
           05  SCB-GRP-0416.
               10  SCB-D-004563             PIC 9(04) VALUE 1941.
               10  SCB-DESC-004563          PIC X(18) VALUE 'DESC-059319'.
               10  SCB-E-004564             OCCURS 2 TIMES.
                   15  SCB-EK-004564        PIC X(10).
                   15  SCB-EV-004564        PIC 9(06) COMP.
               10  SCB-A-004565             PIC 9(09) COMP VALUE 4565.
               10  SCB-B-004566             PIC S9(07)V99 COMP-3 VALUE +0004566.
               10  SCB-C-004567             PIC X(24) VALUE 'FIELD-004567-ALPHA'.
               10  SCB-D-004568             PIC 9(04) VALUE 1976.
               10  SCB-DESC-004568          PIC X(18) VALUE 'DESC-059384'.
               10  SCB-E-004569             OCCURS 3 TIMES.
                   15  SCB-EK-004569        PIC X(10).
                   15  SCB-EV-004569        PIC 9(06) COMP.
               10  SCB-A-004570             PIC 9(09) COMP VALUE 4570.
               10  SCB-B-004571             PIC S9(07)V99 COMP-3 VALUE +0004571.
               10  SCB-C-004572             PIC X(24) VALUE 'FIELD-004572-ALPHA'.
               10  SCB-D-004573             PIC 9(04) VALUE 2011.
               10  SCB-DESC-004573          PIC X(18) VALUE 'DESC-059449'.
           05  SCB-GRP-0417.
               10  SCB-E-004574             OCCURS 4 TIMES.
                   15  SCB-EK-004574        PIC X(10).
                   15  SCB-EV-004574        PIC 9(06) COMP.
               10  SCB-A-004575             PIC 9(09) COMP VALUE 4575.
               10  SCB-B-004576             PIC S9(07)V99 COMP-3 VALUE +0004576.
               10  SCB-C-004577             PIC X(24) VALUE 'FIELD-004577-ALPHA'.
               10  SCB-D-004578             PIC 9(04) VALUE 2046.
               10  SCB-DESC-004578          PIC X(18) VALUE 'DESC-059514'.
               10  SCB-E-004579             OCCURS 5 TIMES.
                   15  SCB-EK-004579        PIC X(10).
                   15  SCB-EV-004579        PIC 9(06) COMP.
               10  SCB-A-004580             PIC 9(09) COMP VALUE 4580.
               10  SCB-B-004581             PIC S9(07)V99 COMP-3 VALUE +0004581.
               10  SCB-C-004582             PIC X(24) VALUE 'FIELD-004582-ALPHA'.
               10  SCB-D-004583             PIC 9(04) VALUE 2081.
               10  SCB-DESC-004583          PIC X(18) VALUE 'DESC-059579'.
               10  SCB-E-004584             OCCURS 2 TIMES.
                   15  SCB-EK-004584        PIC X(10).
                   15  SCB-EV-004584        PIC 9(06) COMP.
               10  SCB-A-004585             PIC 9(09) COMP VALUE 4585.
           05  SCB-GRP-0418.
               10  SCB-B-004586             PIC S9(07)V99 COMP-3 VALUE +0004586.
               10  SCB-C-004587             PIC X(24) VALUE 'FIELD-004587-ALPHA'.
               10  SCB-D-004588             PIC 9(04) VALUE 2116.
               10  SCB-DESC-004588          PIC X(18) VALUE 'DESC-059644'.
               10  SCB-E-004589             OCCURS 3 TIMES.
                   15  SCB-EK-004589        PIC X(10).
                   15  SCB-EV-004589        PIC 9(06) COMP.
               10  SCB-A-004590             PIC 9(09) COMP VALUE 4590.
               10  SCB-B-004591             PIC S9(07)V99 COMP-3 VALUE +0004591.
               10  SCB-C-004592             PIC X(24) VALUE 'FIELD-004592-ALPHA'.
               10  SCB-D-004593             PIC 9(04) VALUE 2151.
               10  SCB-DESC-004593          PIC X(18) VALUE 'DESC-059709'.
               10  SCB-E-004594             OCCURS 4 TIMES.
                   15  SCB-EK-004594        PIC X(10).
                   15  SCB-EV-004594        PIC 9(06) COMP.
               10  SCB-A-004595             PIC 9(09) COMP VALUE 4595.
               10  SCB-B-004596             PIC S9(07)V99 COMP-3 VALUE +0004596.
               10  SCB-C-004597             PIC X(24) VALUE 'FIELD-004597-ALPHA'.
               10  SCB-D-004598             PIC 9(04) VALUE 2186.
               10  SCB-DESC-004598          PIC X(18) VALUE 'DESC-059774'.
           05  SCB-GRP-0419.
               10  SCB-E-004599             OCCURS 5 TIMES.
                   15  SCB-EK-004599        PIC X(10).
                   15  SCB-EV-004599        PIC 9(06) COMP.
               10  SCB-A-004600             PIC 9(09) COMP VALUE 4600.
               10  SCB-B-004601             PIC S9(07)V99 COMP-3 VALUE +0004601.
               10  SCB-C-004602             PIC X(24) VALUE 'FIELD-004602-ALPHA'.
               10  SCB-D-004603             PIC 9(04) VALUE 2221.
               10  SCB-DESC-004603          PIC X(18) VALUE 'DESC-059839'.
               10  SCB-E-004604             OCCURS 2 TIMES.
                   15  SCB-EK-004604        PIC X(10).
                   15  SCB-EV-004604        PIC 9(06) COMP.
               10  SCB-A-004605             PIC 9(09) COMP VALUE 4605.
               10  SCB-B-004606             PIC S9(07)V99 COMP-3 VALUE +0004606.
               10  SCB-C-004607             PIC X(24) VALUE 'FIELD-004607-ALPHA'.
               10  SCB-D-004608             PIC 9(04) VALUE 2256.
               10  SCB-DESC-004608          PIC X(18) VALUE 'DESC-059904'.
               10  SCB-E-004609             OCCURS 3 TIMES.
                   15  SCB-EK-004609        PIC X(10).
                   15  SCB-EV-004609        PIC 9(06) COMP.
               10  SCB-A-004610             PIC 9(09) COMP VALUE 4610.
               10  SCB-B-004611             PIC S9(07)V99 COMP-3 VALUE +0004611.
               10  SCB-C-004612             PIC X(24) VALUE 'FIELD-004612-ALPHA'.
           05  SCB-GRP-0420.
               10  SCB-D-004613             PIC 9(04) VALUE 2291.
               10  SCB-DESC-004613          PIC X(18) VALUE 'DESC-059969'.
               10  SCB-E-004614             OCCURS 4 TIMES.
                   15  SCB-EK-004614        PIC X(10).
                   15  SCB-EV-004614        PIC 9(06) COMP.
               10  SCB-A-004615             PIC 9(09) COMP VALUE 4615.
               10  SCB-B-004616             PIC S9(07)V99 COMP-3 VALUE +0004616.
               10  SCB-C-004617             PIC X(24) VALUE 'FIELD-004617-ALPHA'.
               10  SCB-D-004618             PIC 9(04) VALUE 2326.
               10  SCB-DESC-004618          PIC X(18) VALUE 'DESC-060034'.
               10  SCB-E-004619             OCCURS 5 TIMES.
                   15  SCB-EK-004619        PIC X(10).
                   15  SCB-EV-004619        PIC 9(06) COMP.
               10  SCB-A-004620             PIC 9(09) COMP VALUE 4620.
           05  SCB-ALT-0070 REDEFINES SCB-GRP-0420.
               10  SCB-ALT-FLAG-0070      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0070   PIC X(64).
           05  SCB-GRP-0421.
               10  SCB-B-004621             PIC S9(07)V99 COMP-3 VALUE +0004621.
               10  SCB-C-004622             PIC X(24) VALUE 'FIELD-004622-ALPHA'.
               10  SCB-D-004623             PIC 9(04) VALUE 2361.
               10  SCB-DESC-004623          PIC X(18) VALUE 'DESC-060099'.
               10  SCB-E-004624             OCCURS 2 TIMES.
                   15  SCB-EK-004624        PIC X(10).
                   15  SCB-EV-004624        PIC 9(06) COMP.
               10  SCB-A-004625             PIC 9(09) COMP VALUE 4625.
               10  SCB-B-004626             PIC S9(07)V99 COMP-3 VALUE +0004626.
               10  SCB-C-004627             PIC X(24) VALUE 'FIELD-004627-ALPHA'.
               10  SCB-D-004628             PIC 9(04) VALUE 2396.
               10  SCB-DESC-004628          PIC X(18) VALUE 'DESC-060164'.
               10  SCB-E-004629             OCCURS 3 TIMES.
                   15  SCB-EK-004629        PIC X(10).
                   15  SCB-EV-004629        PIC 9(06) COMP.
           05  SCB-GRP-0422.
               10  SCB-A-004630             PIC 9(09) COMP VALUE 4630.
               10  SCB-B-004631             PIC S9(07)V99 COMP-3 VALUE +0004631.
               10  SCB-C-004632             PIC X(24) VALUE 'FIELD-004632-ALPHA'.
               10  SCB-D-004633             PIC 9(04) VALUE 2431.
               10  SCB-DESC-004633          PIC X(18) VALUE 'DESC-060229'.
               10  SCB-E-004634             OCCURS 4 TIMES.
                   15  SCB-EK-004634        PIC X(10).
                   15  SCB-EV-004634        PIC 9(06) COMP.
               10  SCB-A-004635             PIC 9(09) COMP VALUE 4635.
               10  SCB-B-004636             PIC S9(07)V99 COMP-3 VALUE +0004636.
               10  SCB-C-004637             PIC X(24) VALUE 'FIELD-004637-ALPHA'.
               10  SCB-D-004638             PIC 9(04) VALUE 2466.
               10  SCB-DESC-004638          PIC X(18) VALUE 'DESC-060294'.
               10  SCB-E-004639             OCCURS 5 TIMES.
                   15  SCB-EK-004639        PIC X(10).
                   15  SCB-EV-004639        PIC 9(06) COMP.
           05  SCB-GRP-0423.
               10  SCB-A-004640             PIC 9(09) COMP VALUE 4640.
               10  SCB-B-004641             PIC S9(07)V99 COMP-3 VALUE +0004641.
               10  SCB-C-004642             PIC X(24) VALUE 'FIELD-004642-ALPHA'.
               10  SCB-D-004643             PIC 9(04) VALUE 2501.
               10  SCB-DESC-004643          PIC X(18) VALUE 'DESC-060359'.
               10  SCB-E-004644             OCCURS 2 TIMES.
                   15  SCB-EK-004644        PIC X(10).
                   15  SCB-EV-004644        PIC 9(06) COMP.
               10  SCB-A-004645             PIC 9(09) COMP VALUE 4645.
               10  SCB-B-004646             PIC S9(07)V99 COMP-3 VALUE +0004646.
               10  SCB-C-004647             PIC X(24) VALUE 'FIELD-004647-ALPHA'.
               10  SCB-D-004648             PIC 9(04) VALUE 2536.
               10  SCB-DESC-004648          PIC X(18) VALUE 'DESC-060424'.
               10  SCB-E-004649             OCCURS 3 TIMES.
                   15  SCB-EK-004649        PIC X(10).
                   15  SCB-EV-004649        PIC 9(06) COMP.
               10  SCB-A-004650             PIC 9(09) COMP VALUE 4650.
           05  SCB-GRP-0424.
               10  SCB-B-004651             PIC S9(07)V99 COMP-3 VALUE +0004651.
               10  SCB-C-004652             PIC X(24) VALUE 'FIELD-004652-ALPHA'.
               10  SCB-D-004653             PIC 9(04) VALUE 2571.
               10  SCB-DESC-004653          PIC X(18) VALUE 'DESC-060489'.
               10  SCB-E-004654             OCCURS 4 TIMES.
                   15  SCB-EK-004654        PIC X(10).
                   15  SCB-EV-004654        PIC 9(06) COMP.
               10  SCB-A-004655             PIC 9(09) COMP VALUE 4655.
               10  SCB-B-004656             PIC S9(07)V99 COMP-3 VALUE +0004656.
               10  SCB-C-004657             PIC X(24) VALUE 'FIELD-004657-ALPHA'.
               10  SCB-D-004658             PIC 9(04) VALUE 2606.
               10  SCB-DESC-004658          PIC X(18) VALUE 'DESC-060554'.
               10  SCB-E-004659             OCCURS 5 TIMES.
                   15  SCB-EK-004659        PIC X(10).
                   15  SCB-EV-004659        PIC 9(06) COMP.
               10  SCB-A-004660             PIC 9(09) COMP VALUE 4660.
               10  SCB-B-004661             PIC S9(07)V99 COMP-3 VALUE +0004661.
               10  SCB-C-004662             PIC X(24) VALUE 'FIELD-004662-ALPHA'.
           05  SCB-GRP-0425.
               10  SCB-D-004663             PIC 9(04) VALUE 2641.
               10  SCB-DESC-004663          PIC X(18) VALUE 'DESC-060619'.
               10  SCB-E-004664             OCCURS 2 TIMES.
                   15  SCB-EK-004664        PIC X(10).
                   15  SCB-EV-004664        PIC 9(06) COMP.
               10  SCB-A-004665             PIC 9(09) COMP VALUE 4665.
               10  SCB-B-004666             PIC S9(07)V99 COMP-3 VALUE +0004666.
               10  SCB-C-004667             PIC X(24) VALUE 'FIELD-004667-ALPHA'.
               10  SCB-D-004668             PIC 9(04) VALUE 2676.
               10  SCB-DESC-004668          PIC X(18) VALUE 'DESC-060684'.
               10  SCB-E-004669             OCCURS 3 TIMES.
                   15  SCB-EK-004669        PIC X(10).
                   15  SCB-EV-004669        PIC 9(06) COMP.
               10  SCB-A-004670             PIC 9(09) COMP VALUE 4670.
               10  SCB-B-004671             PIC S9(07)V99 COMP-3 VALUE +0004671.
               10  SCB-C-004672             PIC X(24) VALUE 'FIELD-004672-ALPHA'.
               10  SCB-D-004673             PIC 9(04) VALUE 2711.
               10  SCB-DESC-004673          PIC X(18) VALUE 'DESC-060749'.
               10  SCB-E-004674             OCCURS 4 TIMES.
                   15  SCB-EK-004674        PIC X(10).
                   15  SCB-EV-004674        PIC 9(06) COMP.
               10  SCB-A-004675             PIC 9(09) COMP VALUE 4675.
           05  SCB-GRP-0426.
               10  SCB-B-004676             PIC S9(07)V99 COMP-3 VALUE +0004676.
               10  SCB-C-004677             PIC X(24) VALUE 'FIELD-004677-ALPHA'.
               10  SCB-D-004678             PIC 9(04) VALUE 2746.
               10  SCB-DESC-004678          PIC X(18) VALUE 'DESC-060814'.
               10  SCB-E-004679             OCCURS 5 TIMES.
                   15  SCB-EK-004679        PIC X(10).
                   15  SCB-EV-004679        PIC 9(06) COMP.
               10  SCB-A-004680             PIC 9(09) COMP VALUE 4680.
               10  SCB-B-004681             PIC S9(07)V99 COMP-3 VALUE +0004681.
               10  SCB-C-004682             PIC X(24) VALUE 'FIELD-004682-ALPHA'.
               10  SCB-D-004683             PIC 9(04) VALUE 2781.
               10  SCB-DESC-004683          PIC X(18) VALUE 'DESC-060879'.
               10  SCB-E-004684             OCCURS 2 TIMES.
                   15  SCB-EK-004684        PIC X(10).
                   15  SCB-EV-004684        PIC 9(06) COMP.
               10  SCB-A-004685             PIC 9(09) COMP VALUE 4685.
               10  SCB-B-004686             PIC S9(07)V99 COMP-3 VALUE +0004686.
               10  SCB-C-004687             PIC X(24) VALUE 'FIELD-004687-ALPHA'.
               10  SCB-D-004688             PIC 9(04) VALUE 2816.
               10  SCB-DESC-004688          PIC X(18) VALUE 'DESC-060944'.
               10  SCB-E-004689             OCCURS 3 TIMES.
                   15  SCB-EK-004689        PIC X(10).
                   15  SCB-EV-004689        PIC 9(06) COMP.
           05  SCB-ALT-0071 REDEFINES SCB-GRP-0426.
               10  SCB-ALT-FLAG-0071      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0071   PIC X(64).
           05  SCB-GRP-0427.
               10  SCB-A-004690             PIC 9(09) COMP VALUE 4690.
               10  SCB-B-004691             PIC S9(07)V99 COMP-3 VALUE +0004691.
               10  SCB-C-004692             PIC X(24) VALUE 'FIELD-004692-ALPHA'.
               10  SCB-D-004693             PIC 9(04) VALUE 2851.
               10  SCB-DESC-004693          PIC X(18) VALUE 'DESC-061009'.
               10  SCB-E-004694             OCCURS 4 TIMES.
                   15  SCB-EK-004694        PIC X(10).
                   15  SCB-EV-004694        PIC 9(06) COMP.
               10  SCB-A-004695             PIC 9(09) COMP VALUE 4695.
               10  SCB-B-004696             PIC S9(07)V99 COMP-3 VALUE +0004696.
               10  SCB-C-004697             PIC X(24) VALUE 'FIELD-004697-ALPHA'.
           05  SCB-GRP-0428.
               10  SCB-D-004698             PIC 9(04) VALUE 2886.
               10  SCB-DESC-004698          PIC X(18) VALUE 'DESC-061074'.
               10  SCB-E-004699             OCCURS 5 TIMES.
                   15  SCB-EK-004699        PIC X(10).
                   15  SCB-EV-004699        PIC 9(06) COMP.
               10  SCB-A-004700             PIC 9(09) COMP VALUE 4700.
               10  SCB-B-004701             PIC S9(07)V99 COMP-3 VALUE +0004701.
               10  SCB-C-004702             PIC X(24) VALUE 'FIELD-004702-ALPHA'.
               10  SCB-D-004703             PIC 9(04) VALUE 2921.
               10  SCB-DESC-004703          PIC X(18) VALUE 'DESC-061139'.
               10  SCB-E-004704             OCCURS 2 TIMES.
                   15  SCB-EK-004704        PIC X(10).
                   15  SCB-EV-004704        PIC 9(06) COMP.
               10  SCB-A-004705             PIC 9(09) COMP VALUE 4705.
               10  SCB-B-004706             PIC S9(07)V99 COMP-3 VALUE +0004706.
           05  SCB-GRP-0429.
               10  SCB-C-004707             PIC X(24) VALUE 'FIELD-004707-ALPHA'.
               10  SCB-D-004708             PIC 9(04) VALUE 2956.
               10  SCB-DESC-004708          PIC X(18) VALUE 'DESC-061204'.
               10  SCB-E-004709             OCCURS 3 TIMES.
                   15  SCB-EK-004709        PIC X(10).
                   15  SCB-EV-004709        PIC 9(06) COMP.
               10  SCB-A-004710             PIC 9(09) COMP VALUE 4710.
               10  SCB-B-004711             PIC S9(07)V99 COMP-3 VALUE +0004711.
               10  SCB-C-004712             PIC X(24) VALUE 'FIELD-004712-ALPHA'.
               10  SCB-D-004713             PIC 9(04) VALUE 2991.
               10  SCB-DESC-004713          PIC X(18) VALUE 'DESC-061269'.
               10  SCB-E-004714             OCCURS 4 TIMES.
                   15  SCB-EK-004714        PIC X(10).
                   15  SCB-EV-004714        PIC 9(06) COMP.
               10  SCB-A-004715             PIC 9(09) COMP VALUE 4715.
               10  SCB-B-004716             PIC S9(07)V99 COMP-3 VALUE +0004716.
           05  SCB-GRP-0430.
               10  SCB-C-004717             PIC X(24) VALUE 'FIELD-004717-ALPHA'.
               10  SCB-D-004718             PIC 9(04) VALUE 3026.
               10  SCB-DESC-004718          PIC X(18) VALUE 'DESC-061334'.
               10  SCB-E-004719             OCCURS 5 TIMES.
                   15  SCB-EK-004719        PIC X(10).
                   15  SCB-EV-004719        PIC 9(06) COMP.
               10  SCB-A-004720             PIC 9(09) COMP VALUE 4720.
               10  SCB-B-004721             PIC S9(07)V99 COMP-3 VALUE +0004721.
               10  SCB-C-004722             PIC X(24) VALUE 'FIELD-004722-ALPHA'.
               10  SCB-D-004723             PIC 9(04) VALUE 3061.
               10  SCB-DESC-004723          PIC X(18) VALUE 'DESC-061399'.
               10  SCB-E-004724             OCCURS 2 TIMES.
                   15  SCB-EK-004724        PIC X(10).
                   15  SCB-EV-004724        PIC 9(06) COMP.
               10  SCB-A-004725             PIC 9(09) COMP VALUE 4725.
               10  SCB-B-004726             PIC S9(07)V99 COMP-3 VALUE +0004726.
               10  SCB-C-004727             PIC X(24) VALUE 'FIELD-004727-ALPHA'.
           05  SCB-GRP-0431.
               10  SCB-D-004728             PIC 9(04) VALUE 3096.
               10  SCB-DESC-004728          PIC X(18) VALUE 'DESC-061464'.
               10  SCB-E-004729             OCCURS 3 TIMES.
                   15  SCB-EK-004729        PIC X(10).
                   15  SCB-EV-004729        PIC 9(06) COMP.
               10  SCB-A-004730             PIC 9(09) COMP VALUE 4730.
               10  SCB-B-004731             PIC S9(07)V99 COMP-3 VALUE +0004731.
               10  SCB-C-004732             PIC X(24) VALUE 'FIELD-004732-ALPHA'.
               10  SCB-D-004733             PIC 9(04) VALUE 3131.
               10  SCB-DESC-004733          PIC X(18) VALUE 'DESC-061529'.
               10  SCB-E-004734             OCCURS 4 TIMES.
                   15  SCB-EK-004734        PIC X(10).
                   15  SCB-EV-004734        PIC 9(06) COMP.
               10  SCB-A-004735             PIC 9(09) COMP VALUE 4735.
               10  SCB-B-004736             PIC S9(07)V99 COMP-3 VALUE +0004736.
               10  SCB-C-004737             PIC X(24) VALUE 'FIELD-004737-ALPHA'.
               10  SCB-D-004738             PIC 9(04) VALUE 3166.
               10  SCB-DESC-004738          PIC X(18) VALUE 'DESC-061594'.
               10  SCB-E-004739             OCCURS 5 TIMES.
                   15  SCB-EK-004739        PIC X(10).
                   15  SCB-EV-004739        PIC 9(06) COMP.
           05  SCB-GRP-0432.
               10  SCB-A-004740             PIC 9(09) COMP VALUE 4740.
               10  SCB-B-004741             PIC S9(07)V99 COMP-3 VALUE +0004741.
               10  SCB-C-004742             PIC X(24) VALUE 'FIELD-004742-ALPHA'.
               10  SCB-D-004743             PIC 9(04) VALUE 3201.
               10  SCB-DESC-004743          PIC X(18) VALUE 'DESC-061659'.
               10  SCB-E-004744             OCCURS 2 TIMES.
                   15  SCB-EK-004744        PIC X(10).
                   15  SCB-EV-004744        PIC 9(06) COMP.
               10  SCB-A-004745             PIC 9(09) COMP VALUE 4745.
               10  SCB-B-004746             PIC S9(07)V99 COMP-3 VALUE +0004746.
               10  SCB-C-004747             PIC X(24) VALUE 'FIELD-004747-ALPHA'.
               10  SCB-D-004748             PIC 9(04) VALUE 3236.
               10  SCB-DESC-004748          PIC X(18) VALUE 'DESC-061724'.
               10  SCB-E-004749             OCCURS 3 TIMES.
                   15  SCB-EK-004749        PIC X(10).
                   15  SCB-EV-004749        PIC 9(06) COMP.
               10  SCB-A-004750             PIC 9(09) COMP VALUE 4750.
               10  SCB-B-004751             PIC S9(07)V99 COMP-3 VALUE +0004751.
               10  SCB-C-004752             PIC X(24) VALUE 'FIELD-004752-ALPHA'.
           05  SCB-ALT-0072 REDEFINES SCB-GRP-0432.
               10  SCB-ALT-FLAG-0072      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0072   PIC X(64).
           05  SCB-GRP-0433.
               10  SCB-D-004753             PIC 9(04) VALUE 3271.
               10  SCB-DESC-004753          PIC X(18) VALUE 'DESC-061789'.
               10  SCB-E-004754             OCCURS 4 TIMES.
                   15  SCB-EK-004754        PIC X(10).
                   15  SCB-EV-004754        PIC 9(06) COMP.
               10  SCB-A-004755             PIC 9(09) COMP VALUE 4755.
               10  SCB-B-004756             PIC S9(07)V99 COMP-3 VALUE +0004756.
               10  SCB-C-004757             PIC X(24) VALUE 'FIELD-004757-ALPHA'.
               10  SCB-D-004758             PIC 9(04) VALUE 3306.
               10  SCB-DESC-004758          PIC X(18) VALUE 'DESC-061854'.
               10  SCB-E-004759             OCCURS 5 TIMES.
                   15  SCB-EK-004759        PIC X(10).
                   15  SCB-EV-004759        PIC 9(06) COMP.
               10  SCB-A-004760             PIC 9(09) COMP VALUE 4760.
               10  SCB-B-004761             PIC S9(07)V99 COMP-3 VALUE +0004761.
               10  SCB-C-004762             PIC X(24) VALUE 'FIELD-004762-ALPHA'.
               10  SCB-D-004763             PIC 9(04) VALUE 3341.
               10  SCB-DESC-004763          PIC X(18) VALUE 'DESC-061919'.
               10  SCB-E-004764             OCCURS 2 TIMES.
                   15  SCB-EK-004764        PIC X(10).
                   15  SCB-EV-004764        PIC 9(06) COMP.
               10  SCB-A-004765             PIC 9(09) COMP VALUE 4765.
               10  SCB-B-004766             PIC S9(07)V99 COMP-3 VALUE +0004766.
           05  SCB-GRP-0434.
               10  SCB-C-004767             PIC X(24) VALUE 'FIELD-004767-ALPHA'.
               10  SCB-D-004768             PIC 9(04) VALUE 3376.
               10  SCB-DESC-004768          PIC X(18) VALUE 'DESC-061984'.
               10  SCB-E-004769             OCCURS 3 TIMES.
                   15  SCB-EK-004769        PIC X(10).
                   15  SCB-EV-004769        PIC 9(06) COMP.
               10  SCB-A-004770             PIC 9(09) COMP VALUE 4770.
               10  SCB-B-004771             PIC S9(07)V99 COMP-3 VALUE +0004771.
               10  SCB-C-004772             PIC X(24) VALUE 'FIELD-004772-ALPHA'.
               10  SCB-D-004773             PIC 9(04) VALUE 3411.
               10  SCB-DESC-004773          PIC X(18) VALUE 'DESC-062049'.
               10  SCB-E-004774             OCCURS 4 TIMES.
                   15  SCB-EK-004774        PIC X(10).
                   15  SCB-EV-004774        PIC 9(06) COMP.
           05  SCB-GRP-0435.
               10  SCB-A-004775             PIC 9(09) COMP VALUE 4775.
               10  SCB-B-004776             PIC S9(07)V99 COMP-3 VALUE +0004776.
               10  SCB-C-004777             PIC X(24) VALUE 'FIELD-004777-ALPHA'.
               10  SCB-D-004778             PIC 9(04) VALUE 3446.
               10  SCB-DESC-004778          PIC X(18) VALUE 'DESC-062114'.
               10  SCB-E-004779             OCCURS 5 TIMES.
                   15  SCB-EK-004779        PIC X(10).
                   15  SCB-EV-004779        PIC 9(06) COMP.
               10  SCB-A-004780             PIC 9(09) COMP VALUE 4780.
               10  SCB-B-004781             PIC S9(07)V99 COMP-3 VALUE +0004781.
               10  SCB-C-004782             PIC X(24) VALUE 'FIELD-004782-ALPHA'.
               10  SCB-D-004783             PIC 9(04) VALUE 3481.
               10  SCB-DESC-004783          PIC X(18) VALUE 'DESC-062179'.
           05  SCB-GRP-0436.
               10  SCB-E-004784             OCCURS 2 TIMES.
                   15  SCB-EK-004784        PIC X(10).
                   15  SCB-EV-004784        PIC 9(06) COMP.
               10  SCB-A-004785             PIC 9(09) COMP VALUE 4785.
               10  SCB-B-004786             PIC S9(07)V99 COMP-3 VALUE +0004786.
               10  SCB-C-004787             PIC X(24) VALUE 'FIELD-004787-ALPHA'.
               10  SCB-D-004788             PIC 9(04) VALUE 3516.
               10  SCB-DESC-004788          PIC X(18) VALUE 'DESC-062244'.
               10  SCB-E-004789             OCCURS 3 TIMES.
                   15  SCB-EK-004789        PIC X(10).
                   15  SCB-EV-004789        PIC 9(06) COMP.
               10  SCB-A-004790             PIC 9(09) COMP VALUE 4790.
               10  SCB-B-004791             PIC S9(07)V99 COMP-3 VALUE +0004791.
               10  SCB-C-004792             PIC X(24) VALUE 'FIELD-004792-ALPHA'.
               10  SCB-D-004793             PIC 9(04) VALUE 3551.
               10  SCB-DESC-004793          PIC X(18) VALUE 'DESC-062309'.
           05  SCB-GRP-0437.
               10  SCB-E-004794             OCCURS 4 TIMES.
                   15  SCB-EK-004794        PIC X(10).
                   15  SCB-EV-004794        PIC 9(06) COMP.
               10  SCB-A-004795             PIC 9(09) COMP VALUE 4795.
               10  SCB-B-004796             PIC S9(07)V99 COMP-3 VALUE +0004796.
               10  SCB-C-004797             PIC X(24) VALUE 'FIELD-004797-ALPHA'.
               10  SCB-D-004798             PIC 9(04) VALUE 3586.
               10  SCB-DESC-004798          PIC X(18) VALUE 'DESC-062374'.
               10  SCB-E-004799             OCCURS 5 TIMES.
                   15  SCB-EK-004799        PIC X(10).
                   15  SCB-EV-004799        PIC 9(06) COMP.
               10  SCB-A-004800             PIC 9(09) COMP VALUE 4800.
               10  SCB-B-004801             PIC S9(07)V99 COMP-3 VALUE +0004801.
               10  SCB-C-004802             PIC X(24) VALUE 'FIELD-004802-ALPHA'.
               10  SCB-D-004803             PIC 9(04) VALUE 3621.
               10  SCB-DESC-004803          PIC X(18) VALUE 'DESC-062439'.
               10  SCB-E-004804             OCCURS 2 TIMES.
                   15  SCB-EK-004804        PIC X(10).
                   15  SCB-EV-004804        PIC 9(06) COMP.
           05  SCB-GRP-0438.
               10  SCB-A-004805             PIC 9(09) COMP VALUE 4805.
               10  SCB-B-004806             PIC S9(07)V99 COMP-3 VALUE +0004806.
               10  SCB-C-004807             PIC X(24) VALUE 'FIELD-004807-ALPHA'.
               10  SCB-D-004808             PIC 9(04) VALUE 3656.
               10  SCB-DESC-004808          PIC X(18) VALUE 'DESC-062504'.
               10  SCB-E-004809             OCCURS 3 TIMES.
                   15  SCB-EK-004809        PIC X(10).
                   15  SCB-EV-004809        PIC 9(06) COMP.
               10  SCB-A-004810             PIC 9(09) COMP VALUE 4810.
               10  SCB-B-004811             PIC S9(07)V99 COMP-3 VALUE +0004811.
               10  SCB-C-004812             PIC X(24) VALUE 'FIELD-004812-ALPHA'.
               10  SCB-D-004813             PIC 9(04) VALUE 3691.
               10  SCB-DESC-004813          PIC X(18) VALUE 'DESC-062569'.
               10  SCB-E-004814             OCCURS 4 TIMES.
                   15  SCB-EK-004814        PIC X(10).
                   15  SCB-EV-004814        PIC 9(06) COMP.
               10  SCB-A-004815             PIC 9(09) COMP VALUE 4815.
               10  SCB-B-004816             PIC S9(07)V99 COMP-3 VALUE +0004816.
           05  SCB-ALT-0073 REDEFINES SCB-GRP-0438.
               10  SCB-ALT-FLAG-0073      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0073   PIC X(64).
           05  SCB-GRP-0439.
               10  SCB-C-004817             PIC X(24) VALUE 'FIELD-004817-ALPHA'.
               10  SCB-D-004818             PIC 9(04) VALUE 3726.
               10  SCB-DESC-004818          PIC X(18) VALUE 'DESC-062634'.
               10  SCB-E-004819             OCCURS 5 TIMES.
                   15  SCB-EK-004819        PIC X(10).
                   15  SCB-EV-004819        PIC 9(06) COMP.
               10  SCB-A-004820             PIC 9(09) COMP VALUE 4820.
               10  SCB-B-004821             PIC S9(07)V99 COMP-3 VALUE +0004821.
               10  SCB-C-004822             PIC X(24) VALUE 'FIELD-004822-ALPHA'.
               10  SCB-D-004823             PIC 9(04) VALUE 3761.
               10  SCB-DESC-004823          PIC X(18) VALUE 'DESC-062699'.
               10  SCB-E-004824             OCCURS 2 TIMES.
                   15  SCB-EK-004824        PIC X(10).
                   15  SCB-EV-004824        PIC 9(06) COMP.
               10  SCB-A-004825             PIC 9(09) COMP VALUE 4825.
               10  SCB-B-004826             PIC S9(07)V99 COMP-3 VALUE +0004826.
               10  SCB-C-004827             PIC X(24) VALUE 'FIELD-004827-ALPHA'.
               10  SCB-D-004828             PIC 9(04) VALUE 3796.
               10  SCB-DESC-004828          PIC X(18) VALUE 'DESC-062764'.
               10  SCB-E-004829             OCCURS 3 TIMES.
                   15  SCB-EK-004829        PIC X(10).
                   15  SCB-EV-004829        PIC 9(06) COMP.
           05  SCB-GRP-0440.
               10  SCB-A-004830             PIC 9(09) COMP VALUE 4830.
               10  SCB-B-004831             PIC S9(07)V99 COMP-3 VALUE +0004831.
               10  SCB-C-004832             PIC X(24) VALUE 'FIELD-004832-ALPHA'.
               10  SCB-D-004833             PIC 9(04) VALUE 3831.
               10  SCB-DESC-004833          PIC X(18) VALUE 'DESC-062829'.
               10  SCB-E-004834             OCCURS 4 TIMES.
                   15  SCB-EK-004834        PIC X(10).
                   15  SCB-EV-004834        PIC 9(06) COMP.
               10  SCB-A-004835             PIC 9(09) COMP VALUE 4835.
               10  SCB-B-004836             PIC S9(07)V99 COMP-3 VALUE +0004836.
               10  SCB-C-004837             PIC X(24) VALUE 'FIELD-004837-ALPHA'.
               10  SCB-D-004838             PIC 9(04) VALUE 3866.
               10  SCB-DESC-004838          PIC X(18) VALUE 'DESC-062894'.
               10  SCB-E-004839             OCCURS 5 TIMES.
                   15  SCB-EK-004839        PIC X(10).
                   15  SCB-EV-004839        PIC 9(06) COMP.
               10  SCB-A-004840             PIC 9(09) COMP VALUE 4840.
               10  SCB-B-004841             PIC S9(07)V99 COMP-3 VALUE +0004841.
               10  SCB-C-004842             PIC X(24) VALUE 'FIELD-004842-ALPHA'.
               10  SCB-D-004843             PIC 9(04) VALUE 3901.
               10  SCB-DESC-004843          PIC X(18) VALUE 'DESC-062959'.
           05  SCB-GRP-0441.
               10  SCB-E-004844             OCCURS 2 TIMES.
                   15  SCB-EK-004844        PIC X(10).
                   15  SCB-EV-004844        PIC 9(06) COMP.
               10  SCB-A-004845             PIC 9(09) COMP VALUE 4845.
               10  SCB-B-004846             PIC S9(07)V99 COMP-3 VALUE +0004846.
               10  SCB-C-004847             PIC X(24) VALUE 'FIELD-004847-ALPHA'.
               10  SCB-D-004848             PIC 9(04) VALUE 3936.
               10  SCB-DESC-004848          PIC X(18) VALUE 'DESC-063024'.
               10  SCB-E-004849             OCCURS 3 TIMES.
                   15  SCB-EK-004849        PIC X(10).
                   15  SCB-EV-004849        PIC 9(06) COMP.
               10  SCB-A-004850             PIC 9(09) COMP VALUE 4850.
               10  SCB-B-004851             PIC S9(07)V99 COMP-3 VALUE +0004851.
           05  SCB-GRP-0442.
               10  SCB-C-004852             PIC X(24) VALUE 'FIELD-004852-ALPHA'.
               10  SCB-D-004853             PIC 9(04) VALUE 3971.
               10  SCB-DESC-004853          PIC X(18) VALUE 'DESC-063089'.
               10  SCB-E-004854             OCCURS 4 TIMES.
                   15  SCB-EK-004854        PIC X(10).
                   15  SCB-EV-004854        PIC 9(06) COMP.
               10  SCB-A-004855             PIC 9(09) COMP VALUE 4855.
               10  SCB-B-004856             PIC S9(07)V99 COMP-3 VALUE +0004856.
               10  SCB-C-004857             PIC X(24) VALUE 'FIELD-004857-ALPHA'.
               10  SCB-D-004858             PIC 9(04) VALUE 4006.
               10  SCB-DESC-004858          PIC X(18) VALUE 'DESC-063154'.
               10  SCB-E-004859             OCCURS 5 TIMES.
                   15  SCB-EK-004859        PIC X(10).
                   15  SCB-EV-004859        PIC 9(06) COMP.
               10  SCB-A-004860             PIC 9(09) COMP VALUE 4860.
           05  SCB-GRP-0443.
               10  SCB-B-004861             PIC S9(07)V99 COMP-3 VALUE +0004861.
               10  SCB-C-004862             PIC X(24) VALUE 'FIELD-004862-ALPHA'.
               10  SCB-D-004863             PIC 9(04) VALUE 4041.
               10  SCB-DESC-004863          PIC X(18) VALUE 'DESC-063219'.
               10  SCB-E-004864             OCCURS 2 TIMES.
                   15  SCB-EK-004864        PIC X(10).
                   15  SCB-EV-004864        PIC 9(06) COMP.
               10  SCB-A-004865             PIC 9(09) COMP VALUE 4865.
               10  SCB-B-004866             PIC S9(07)V99 COMP-3 VALUE +0004866.
               10  SCB-C-004867             PIC X(24) VALUE 'FIELD-004867-ALPHA'.
               10  SCB-D-004868             PIC 9(04) VALUE 4076.
               10  SCB-DESC-004868          PIC X(18) VALUE 'DESC-063284'.
               10  SCB-E-004869             OCCURS 3 TIMES.
                   15  SCB-EK-004869        PIC X(10).
                   15  SCB-EV-004869        PIC 9(06) COMP.
               10  SCB-A-004870             PIC 9(09) COMP VALUE 4870.
           05  SCB-GRP-0444.
               10  SCB-B-004871             PIC S9(07)V99 COMP-3 VALUE +0004871.
               10  SCB-C-004872             PIC X(24) VALUE 'FIELD-004872-ALPHA'.
               10  SCB-D-004873             PIC 9(04) VALUE 4111.
               10  SCB-DESC-004873          PIC X(18) VALUE 'DESC-063349'.
               10  SCB-E-004874             OCCURS 4 TIMES.
                   15  SCB-EK-004874        PIC X(10).
                   15  SCB-EV-004874        PIC 9(06) COMP.
               10  SCB-A-004875             PIC 9(09) COMP VALUE 4875.
               10  SCB-B-004876             PIC S9(07)V99 COMP-3 VALUE +0004876.
               10  SCB-C-004877             PIC X(24) VALUE 'FIELD-004877-ALPHA'.
               10  SCB-D-004878             PIC 9(04) VALUE 4146.
               10  SCB-DESC-004878          PIC X(18) VALUE 'DESC-063414'.
               10  SCB-E-004879             OCCURS 5 TIMES.
                   15  SCB-EK-004879        PIC X(10).
                   15  SCB-EV-004879        PIC 9(06) COMP.
               10  SCB-A-004880             PIC 9(09) COMP VALUE 4880.
               10  SCB-B-004881             PIC S9(07)V99 COMP-3 VALUE +0004881.
           05  SCB-ALT-0074 REDEFINES SCB-GRP-0444.
               10  SCB-ALT-FLAG-0074      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0074   PIC X(64).
           05  SCB-GRP-0445.
               10  SCB-C-004882             PIC X(24) VALUE 'FIELD-004882-ALPHA'.
               10  SCB-D-004883             PIC 9(04) VALUE 4181.
               10  SCB-DESC-004883          PIC X(18) VALUE 'DESC-063479'.
               10  SCB-E-004884             OCCURS 2 TIMES.
                   15  SCB-EK-004884        PIC X(10).
                   15  SCB-EV-004884        PIC 9(06) COMP.
               10  SCB-A-004885             PIC 9(09) COMP VALUE 4885.
               10  SCB-B-004886             PIC S9(07)V99 COMP-3 VALUE +0004886.
               10  SCB-C-004887             PIC X(24) VALUE 'FIELD-004887-ALPHA'.
               10  SCB-D-004888             PIC 9(04) VALUE 4216.
               10  SCB-DESC-004888          PIC X(18) VALUE 'DESC-063544'.
               10  SCB-E-004889             OCCURS 3 TIMES.
                   15  SCB-EK-004889        PIC X(10).
                   15  SCB-EV-004889        PIC 9(06) COMP.
               10  SCB-A-004890             PIC 9(09) COMP VALUE 4890.
               10  SCB-B-004891             PIC S9(07)V99 COMP-3 VALUE +0004891.
               10  SCB-C-004892             PIC X(24) VALUE 'FIELD-004892-ALPHA'.
               10  SCB-D-004893             PIC 9(04) VALUE 4251.
               10  SCB-DESC-004893          PIC X(18) VALUE 'DESC-063609'.
           05  SCB-GRP-0446.
               10  SCB-E-004894             OCCURS 4 TIMES.
                   15  SCB-EK-004894        PIC X(10).
                   15  SCB-EV-004894        PIC 9(06) COMP.
               10  SCB-A-004895             PIC 9(09) COMP VALUE 4895.
               10  SCB-B-004896             PIC S9(07)V99 COMP-3 VALUE +0004896.
               10  SCB-C-004897             PIC X(24) VALUE 'FIELD-004897-ALPHA'.
               10  SCB-D-004898             PIC 9(04) VALUE 4286.
               10  SCB-DESC-004898          PIC X(18) VALUE 'DESC-063674'.
               10  SCB-E-004899             OCCURS 5 TIMES.
                   15  SCB-EK-004899        PIC X(10).
                   15  SCB-EV-004899        PIC 9(06) COMP.
               10  SCB-A-004900             PIC 9(09) COMP VALUE 4900.
               10  SCB-B-004901             PIC S9(07)V99 COMP-3 VALUE +0004901.
               10  SCB-C-004902             PIC X(24) VALUE 'FIELD-004902-ALPHA'.
               10  SCB-D-004903             PIC 9(04) VALUE 4321.
               10  SCB-DESC-004903          PIC X(18) VALUE 'DESC-063739'.
               10  SCB-E-004904             OCCURS 2 TIMES.
                   15  SCB-EK-004904        PIC X(10).
                   15  SCB-EV-004904        PIC 9(06) COMP.
               10  SCB-A-004905             PIC 9(09) COMP VALUE 4905.
               10  SCB-B-004906             PIC S9(07)V99 COMP-3 VALUE +0004906.
           05  SCB-GRP-0447.
               10  SCB-C-004907             PIC X(24) VALUE 'FIELD-004907-ALPHA'.
               10  SCB-D-004908             PIC 9(04) VALUE 4356.
               10  SCB-DESC-004908          PIC X(18) VALUE 'DESC-063804'.
               10  SCB-E-004909             OCCURS 3 TIMES.
                   15  SCB-EK-004909        PIC X(10).
                   15  SCB-EV-004909        PIC 9(06) COMP.
               10  SCB-A-004910             PIC 9(09) COMP VALUE 4910.
               10  SCB-B-004911             PIC S9(07)V99 COMP-3 VALUE +0004911.
               10  SCB-C-004912             PIC X(24) VALUE 'FIELD-004912-ALPHA'.
               10  SCB-D-004913             PIC 9(04) VALUE 4391.
               10  SCB-DESC-004913          PIC X(18) VALUE 'DESC-063869'.
               10  SCB-E-004914             OCCURS 4 TIMES.
                   15  SCB-EK-004914        PIC X(10).
                   15  SCB-EV-004914        PIC 9(06) COMP.
               10  SCB-A-004915             PIC 9(09) COMP VALUE 4915.
               10  SCB-B-004916             PIC S9(07)V99 COMP-3 VALUE +0004916.
               10  SCB-C-004917             PIC X(24) VALUE 'FIELD-004917-ALPHA'.
               10  SCB-D-004918             PIC 9(04) VALUE 4426.
               10  SCB-DESC-004918          PIC X(18) VALUE 'DESC-063934'.
               10  SCB-E-004919             OCCURS 5 TIMES.
                   15  SCB-EK-004919        PIC X(10).
                   15  SCB-EV-004919        PIC 9(06) COMP.
               10  SCB-A-004920             PIC 9(09) COMP VALUE 4920.
           05  SCB-GRP-0448.
               10  SCB-B-004921             PIC S9(07)V99 COMP-3 VALUE +0004921.
               10  SCB-C-004922             PIC X(24) VALUE 'FIELD-004922-ALPHA'.
               10  SCB-D-004923             PIC 9(04) VALUE 4461.
               10  SCB-DESC-004923          PIC X(18) VALUE 'DESC-063999'.
               10  SCB-E-004924             OCCURS 2 TIMES.
                   15  SCB-EK-004924        PIC X(10).
                   15  SCB-EV-004924        PIC 9(06) COMP.
               10  SCB-A-004925             PIC 9(09) COMP VALUE 4925.
               10  SCB-B-004926             PIC S9(07)V99 COMP-3 VALUE +0004926.
               10  SCB-C-004927             PIC X(24) VALUE 'FIELD-004927-ALPHA'.
               10  SCB-D-004928             PIC 9(04) VALUE 4496.
               10  SCB-DESC-004928          PIC X(18) VALUE 'DESC-064064'.
           05  SCB-GRP-0449.
               10  SCB-E-004929             OCCURS 3 TIMES.
                   15  SCB-EK-004929        PIC X(10).
                   15  SCB-EV-004929        PIC 9(06) COMP.
               10  SCB-A-004930             PIC 9(09) COMP VALUE 4930.
               10  SCB-B-004931             PIC S9(07)V99 COMP-3 VALUE +0004931.
               10  SCB-C-004932             PIC X(24) VALUE 'FIELD-004932-ALPHA'.
               10  SCB-D-004933             PIC 9(04) VALUE 4531.
               10  SCB-DESC-004933          PIC X(18) VALUE 'DESC-064129'.
               10  SCB-E-004934             OCCURS 4 TIMES.
                   15  SCB-EK-004934        PIC X(10).
                   15  SCB-EV-004934        PIC 9(06) COMP.
               10  SCB-A-004935             PIC 9(09) COMP VALUE 4935.
               10  SCB-B-004936             PIC S9(07)V99 COMP-3 VALUE +0004936.
               10  SCB-C-004937             PIC X(24) VALUE 'FIELD-004937-ALPHA'.
           05  SCB-GRP-0450.
               10  SCB-D-004938             PIC 9(04) VALUE 4566.
               10  SCB-DESC-004938          PIC X(18) VALUE 'DESC-064194'.
               10  SCB-E-004939             OCCURS 5 TIMES.
                   15  SCB-EK-004939        PIC X(10).
                   15  SCB-EV-004939        PIC 9(06) COMP.
               10  SCB-A-004940             PIC 9(09) COMP VALUE 4940.
               10  SCB-B-004941             PIC S9(07)V99 COMP-3 VALUE +0004941.
               10  SCB-C-004942             PIC X(24) VALUE 'FIELD-004942-ALPHA'.
               10  SCB-D-004943             PIC 9(04) VALUE 4601.
               10  SCB-DESC-004943          PIC X(18) VALUE 'DESC-064259'.
               10  SCB-E-004944             OCCURS 2 TIMES.
                   15  SCB-EK-004944        PIC X(10).
                   15  SCB-EV-004944        PIC 9(06) COMP.
               10  SCB-A-004945             PIC 9(09) COMP VALUE 4945.
               10  SCB-B-004946             PIC S9(07)V99 COMP-3 VALUE +0004946.
               10  SCB-C-004947             PIC X(24) VALUE 'FIELD-004947-ALPHA'.
           05  SCB-ALT-0075 REDEFINES SCB-GRP-0450.
               10  SCB-ALT-FLAG-0075      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0075   PIC X(64).
           05  SCB-GRP-0451.
               10  SCB-D-004948             PIC 9(04) VALUE 4636.
               10  SCB-DESC-004948          PIC X(18) VALUE 'DESC-064324'.
               10  SCB-E-004949             OCCURS 3 TIMES.
                   15  SCB-EK-004949        PIC X(10).
                   15  SCB-EV-004949        PIC 9(06) COMP.
               10  SCB-A-004950             PIC 9(09) COMP VALUE 4950.
               10  SCB-B-004951             PIC S9(07)V99 COMP-3 VALUE +0004951.
               10  SCB-C-004952             PIC X(24) VALUE 'FIELD-004952-ALPHA'.
               10  SCB-D-004953             PIC 9(04) VALUE 4671.
               10  SCB-DESC-004953          PIC X(18) VALUE 'DESC-064389'.
               10  SCB-E-004954             OCCURS 4 TIMES.
                   15  SCB-EK-004954        PIC X(10).
                   15  SCB-EV-004954        PIC 9(06) COMP.
               10  SCB-A-004955             PIC 9(09) COMP VALUE 4955.
               10  SCB-B-004956             PIC S9(07)V99 COMP-3 VALUE +0004956.
               10  SCB-C-004957             PIC X(24) VALUE 'FIELD-004957-ALPHA'.
               10  SCB-D-004958             PIC 9(04) VALUE 4706.
               10  SCB-DESC-004958          PIC X(18) VALUE 'DESC-064454'.
           05  SCB-GRP-0452.
               10  SCB-E-004959             OCCURS 5 TIMES.
                   15  SCB-EK-004959        PIC X(10).
                   15  SCB-EV-004959        PIC 9(06) COMP.
               10  SCB-A-004960             PIC 9(09) COMP VALUE 4960.
               10  SCB-B-004961             PIC S9(07)V99 COMP-3 VALUE +0004961.
               10  SCB-C-004962             PIC X(24) VALUE 'FIELD-004962-ALPHA'.
               10  SCB-D-004963             PIC 9(04) VALUE 4741.
               10  SCB-DESC-004963          PIC X(18) VALUE 'DESC-064519'.
               10  SCB-E-004964             OCCURS 2 TIMES.
                   15  SCB-EK-004964        PIC X(10).
                   15  SCB-EV-004964        PIC 9(06) COMP.
               10  SCB-A-004965             PIC 9(09) COMP VALUE 4965.
               10  SCB-B-004966             PIC S9(07)V99 COMP-3 VALUE +0004966.
               10  SCB-C-004967             PIC X(24) VALUE 'FIELD-004967-ALPHA'.
               10  SCB-D-004968             PIC 9(04) VALUE 4776.
               10  SCB-DESC-004968          PIC X(18) VALUE 'DESC-064584'.
               10  SCB-E-004969             OCCURS 3 TIMES.
                   15  SCB-EK-004969        PIC X(10).
                   15  SCB-EV-004969        PIC 9(06) COMP.
               10  SCB-A-004970             PIC 9(09) COMP VALUE 4970.
           05  SCB-GRP-0453.
               10  SCB-B-004971             PIC S9(07)V99 COMP-3 VALUE +0004971.
               10  SCB-C-004972             PIC X(24) VALUE 'FIELD-004972-ALPHA'.
               10  SCB-D-004973             PIC 9(04) VALUE 4811.
               10  SCB-DESC-004973          PIC X(18) VALUE 'DESC-064649'.
               10  SCB-E-004974             OCCURS 4 TIMES.
                   15  SCB-EK-004974        PIC X(10).
                   15  SCB-EV-004974        PIC 9(06) COMP.
               10  SCB-A-004975             PIC 9(09) COMP VALUE 4975.
               10  SCB-B-004976             PIC S9(07)V99 COMP-3 VALUE +0004976.
               10  SCB-C-004977             PIC X(24) VALUE 'FIELD-004977-ALPHA'.
               10  SCB-D-004978             PIC 9(04) VALUE 4846.
               10  SCB-DESC-004978          PIC X(18) VALUE 'DESC-064714'.
               10  SCB-E-004979             OCCURS 5 TIMES.
                   15  SCB-EK-004979        PIC X(10).
                   15  SCB-EV-004979        PIC 9(06) COMP.
               10  SCB-A-004980             PIC 9(09) COMP VALUE 4980.
               10  SCB-B-004981             PIC S9(07)V99 COMP-3 VALUE +0004981.
               10  SCB-C-004982             PIC X(24) VALUE 'FIELD-004982-ALPHA'.
               10  SCB-D-004983             PIC 9(04) VALUE 4881.
               10  SCB-DESC-004983          PIC X(18) VALUE 'DESC-064779'.
           05  SCB-GRP-0454.
               10  SCB-E-004984             OCCURS 2 TIMES.
                   15  SCB-EK-004984        PIC X(10).
                   15  SCB-EV-004984        PIC 9(06) COMP.
               10  SCB-A-004985             PIC 9(09) COMP VALUE 4985.
               10  SCB-B-004986             PIC S9(07)V99 COMP-3 VALUE +0004986.
               10  SCB-C-004987             PIC X(24) VALUE 'FIELD-004987-ALPHA'.
               10  SCB-D-004988             PIC 9(04) VALUE 4916.
               10  SCB-DESC-004988          PIC X(18) VALUE 'DESC-064844'.
               10  SCB-E-004989             OCCURS 3 TIMES.
                   15  SCB-EK-004989        PIC X(10).
                   15  SCB-EV-004989        PIC 9(06) COMP.
               10  SCB-A-004990             PIC 9(09) COMP VALUE 4990.
               10  SCB-B-004991             PIC S9(07)V99 COMP-3 VALUE +0004991.
               10  SCB-C-004992             PIC X(24) VALUE 'FIELD-004992-ALPHA'.
               10  SCB-D-004993             PIC 9(04) VALUE 4951.
               10  SCB-DESC-004993          PIC X(18) VALUE 'DESC-064909'.
               10  SCB-E-004994             OCCURS 4 TIMES.
                   15  SCB-EK-004994        PIC X(10).
                   15  SCB-EV-004994        PIC 9(06) COMP.
               10  SCB-A-004995             PIC 9(09) COMP VALUE 4995.
               10  SCB-B-004996             PIC S9(07)V99 COMP-3 VALUE +0004996.
               10  SCB-C-004997             PIC X(24) VALUE 'FIELD-004997-ALPHA'.
           05  SCB-GRP-0455.
               10  SCB-D-004998             PIC 9(04) VALUE 4986.
               10  SCB-DESC-004998          PIC X(18) VALUE 'DESC-064974'.
               10  SCB-E-004999             OCCURS 5 TIMES.
                   15  SCB-EK-004999        PIC X(10).
                   15  SCB-EV-004999        PIC 9(06) COMP.
               10  SCB-A-005000             PIC 9(09) COMP VALUE 5000.
               10  SCB-B-005001             PIC S9(07)V99 COMP-3 VALUE +0005001.
               10  SCB-C-005002             PIC X(24) VALUE 'FIELD-005002-ALPHA'.
               10  SCB-D-005003             PIC 9(04) VALUE 5021.
               10  SCB-DESC-005003          PIC X(18) VALUE 'DESC-065039'.
               10  SCB-E-005004             OCCURS 2 TIMES.
                   15  SCB-EK-005004        PIC X(10).
                   15  SCB-EV-005004        PIC 9(06) COMP.
               10  SCB-A-005005             PIC 9(09) COMP VALUE 5005.
           05  SCB-GRP-0456.
               10  SCB-B-005006             PIC S9(07)V99 COMP-3 VALUE +0005006.
               10  SCB-C-005007             PIC X(24) VALUE 'FIELD-005007-ALPHA'.
               10  SCB-D-005008             PIC 9(04) VALUE 5056.
               10  SCB-DESC-005008          PIC X(18) VALUE 'DESC-065104'.
               10  SCB-E-005009             OCCURS 3 TIMES.
                   15  SCB-EK-005009        PIC X(10).
                   15  SCB-EV-005009        PIC 9(06) COMP.
               10  SCB-A-005010             PIC 9(09) COMP VALUE 5010.
               10  SCB-B-005011             PIC S9(07)V99 COMP-3 VALUE +0005011.
               10  SCB-C-005012             PIC X(24) VALUE 'FIELD-005012-ALPHA'.
               10  SCB-D-005013             PIC 9(04) VALUE 5091.
               10  SCB-DESC-005013          PIC X(18) VALUE 'DESC-065169'.
               10  SCB-E-005014             OCCURS 4 TIMES.
                   15  SCB-EK-005014        PIC X(10).
                   15  SCB-EV-005014        PIC 9(06) COMP.
           05  SCB-ALT-0076 REDEFINES SCB-GRP-0456.
               10  SCB-ALT-FLAG-0076      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0076   PIC X(64).
           05  SCB-GRP-0457.
               10  SCB-A-005015             PIC 9(09) COMP VALUE 5015.
               10  SCB-B-005016             PIC S9(07)V99 COMP-3 VALUE +0005016.
               10  SCB-C-005017             PIC X(24) VALUE 'FIELD-005017-ALPHA'.
               10  SCB-D-005018             PIC 9(04) VALUE 5126.
               10  SCB-DESC-005018          PIC X(18) VALUE 'DESC-065234'.
               10  SCB-E-005019             OCCURS 5 TIMES.
                   15  SCB-EK-005019        PIC X(10).
                   15  SCB-EV-005019        PIC 9(06) COMP.
               10  SCB-A-005020             PIC 9(09) COMP VALUE 5020.
               10  SCB-B-005021             PIC S9(07)V99 COMP-3 VALUE +0005021.
               10  SCB-C-005022             PIC X(24) VALUE 'FIELD-005022-ALPHA'.
               10  SCB-D-005023             PIC 9(04) VALUE 5161.
               10  SCB-DESC-005023          PIC X(18) VALUE 'DESC-065299'.
               10  SCB-E-005024             OCCURS 2 TIMES.
                   15  SCB-EK-005024        PIC X(10).
                   15  SCB-EV-005024        PIC 9(06) COMP.
           05  SCB-GRP-0458.
               10  SCB-A-005025             PIC 9(09) COMP VALUE 5025.
               10  SCB-B-005026             PIC S9(07)V99 COMP-3 VALUE +0005026.
               10  SCB-C-005027             PIC X(24) VALUE 'FIELD-005027-ALPHA'.
               10  SCB-D-005028             PIC 9(04) VALUE 5196.
               10  SCB-DESC-005028          PIC X(18) VALUE 'DESC-065364'.
               10  SCB-E-005029             OCCURS 3 TIMES.
                   15  SCB-EK-005029        PIC X(10).
                   15  SCB-EV-005029        PIC 9(06) COMP.
               10  SCB-A-005030             PIC 9(09) COMP VALUE 5030.
               10  SCB-B-005031             PIC S9(07)V99 COMP-3 VALUE +0005031.
               10  SCB-C-005032             PIC X(24) VALUE 'FIELD-005032-ALPHA'.
               10  SCB-D-005033             PIC 9(04) VALUE 5231.
               10  SCB-DESC-005033          PIC X(18) VALUE 'DESC-065429'.
               10  SCB-E-005034             OCCURS 4 TIMES.
                   15  SCB-EK-005034        PIC X(10).
                   15  SCB-EV-005034        PIC 9(06) COMP.
               10  SCB-A-005035             PIC 9(09) COMP VALUE 5035.
           05  SCB-GRP-0459.
               10  SCB-B-005036             PIC S9(07)V99 COMP-3 VALUE +0005036.
               10  SCB-C-005037             PIC X(24) VALUE 'FIELD-005037-ALPHA'.
               10  SCB-D-005038             PIC 9(04) VALUE 5266.
               10  SCB-DESC-005038          PIC X(18) VALUE 'DESC-065494'.
               10  SCB-E-005039             OCCURS 5 TIMES.
                   15  SCB-EK-005039        PIC X(10).
                   15  SCB-EV-005039        PIC 9(06) COMP.
               10  SCB-A-005040             PIC 9(09) COMP VALUE 5040.
               10  SCB-B-005041             PIC S9(07)V99 COMP-3 VALUE +0005041.
               10  SCB-C-005042             PIC X(24) VALUE 'FIELD-005042-ALPHA'.
               10  SCB-D-005043             PIC 9(04) VALUE 5301.
               10  SCB-DESC-005043          PIC X(18) VALUE 'DESC-065559'.
               10  SCB-E-005044             OCCURS 2 TIMES.
                   15  SCB-EK-005044        PIC X(10).
                   15  SCB-EV-005044        PIC 9(06) COMP.
               10  SCB-A-005045             PIC 9(09) COMP VALUE 5045.
               10  SCB-B-005046             PIC S9(07)V99 COMP-3 VALUE +0005046.
               10  SCB-C-005047             PIC X(24) VALUE 'FIELD-005047-ALPHA'.
           05  SCB-GRP-0460.
               10  SCB-D-005048             PIC 9(04) VALUE 5336.
               10  SCB-DESC-005048          PIC X(18) VALUE 'DESC-065624'.
               10  SCB-E-005049             OCCURS 3 TIMES.
                   15  SCB-EK-005049        PIC X(10).
                   15  SCB-EV-005049        PIC 9(06) COMP.
               10  SCB-A-005050             PIC 9(09) COMP VALUE 5050.
               10  SCB-B-005051             PIC S9(07)V99 COMP-3 VALUE +0005051.
               10  SCB-C-005052             PIC X(24) VALUE 'FIELD-005052-ALPHA'.
               10  SCB-D-005053             PIC 9(04) VALUE 5371.
               10  SCB-DESC-005053          PIC X(18) VALUE 'DESC-065689'.
               10  SCB-E-005054             OCCURS 4 TIMES.
                   15  SCB-EK-005054        PIC X(10).
                   15  SCB-EV-005054        PIC 9(06) COMP.
               10  SCB-A-005055             PIC 9(09) COMP VALUE 5055.
               10  SCB-B-005056             PIC S9(07)V99 COMP-3 VALUE +0005056.
               10  SCB-C-005057             PIC X(24) VALUE 'FIELD-005057-ALPHA'.
               10  SCB-D-005058             PIC 9(04) VALUE 5406.
               10  SCB-DESC-005058          PIC X(18) VALUE 'DESC-065754'.
               10  SCB-E-005059             OCCURS 5 TIMES.
                   15  SCB-EK-005059        PIC X(10).
                   15  SCB-EV-005059        PIC 9(06) COMP.
               10  SCB-A-005060             PIC 9(09) COMP VALUE 5060.
           05  SCB-GRP-0461.
               10  SCB-B-005061             PIC S9(07)V99 COMP-3 VALUE +0005061.
               10  SCB-C-005062             PIC X(24) VALUE 'FIELD-005062-ALPHA'.
               10  SCB-D-005063             PIC 9(04) VALUE 5441.
               10  SCB-DESC-005063          PIC X(18) VALUE 'DESC-065819'.
               10  SCB-E-005064             OCCURS 2 TIMES.
                   15  SCB-EK-005064        PIC X(10).
                   15  SCB-EV-005064        PIC 9(06) COMP.
               10  SCB-A-005065             PIC 9(09) COMP VALUE 5065.
               10  SCB-B-005066             PIC S9(07)V99 COMP-3 VALUE +0005066.
               10  SCB-C-005067             PIC X(24) VALUE 'FIELD-005067-ALPHA'.
               10  SCB-D-005068             PIC 9(04) VALUE 5476.
               10  SCB-DESC-005068          PIC X(18) VALUE 'DESC-065884'.
               10  SCB-E-005069             OCCURS 3 TIMES.
                   15  SCB-EK-005069        PIC X(10).
                   15  SCB-EV-005069        PIC 9(06) COMP.
               10  SCB-A-005070             PIC 9(09) COMP VALUE 5070.
               10  SCB-B-005071             PIC S9(07)V99 COMP-3 VALUE +0005071.
               10  SCB-C-005072             PIC X(24) VALUE 'FIELD-005072-ALPHA'.
               10  SCB-D-005073             PIC 9(04) VALUE 5511.
               10  SCB-DESC-005073          PIC X(18) VALUE 'DESC-065949'.
               10  SCB-E-005074             OCCURS 4 TIMES.
                   15  SCB-EK-005074        PIC X(10).
                   15  SCB-EV-005074        PIC 9(06) COMP.
           05  SCB-GRP-0462.
               10  SCB-A-005075             PIC 9(09) COMP VALUE 5075.
               10  SCB-B-005076             PIC S9(07)V99 COMP-3 VALUE +0005076.
               10  SCB-C-005077             PIC X(24) VALUE 'FIELD-005077-ALPHA'.
               10  SCB-D-005078             PIC 9(04) VALUE 5546.
               10  SCB-DESC-005078          PIC X(18) VALUE 'DESC-066014'.
               10  SCB-E-005079             OCCURS 5 TIMES.
                   15  SCB-EK-005079        PIC X(10).
                   15  SCB-EV-005079        PIC 9(06) COMP.
               10  SCB-A-005080             PIC 9(09) COMP VALUE 5080.
               10  SCB-B-005081             PIC S9(07)V99 COMP-3 VALUE +0005081.
               10  SCB-C-005082             PIC X(24) VALUE 'FIELD-005082-ALPHA'.
           05  SCB-ALT-0077 REDEFINES SCB-GRP-0462.
               10  SCB-ALT-FLAG-0077      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0077   PIC X(64).
           05  SCB-GRP-0463.
               10  SCB-D-005083             PIC 9(04) VALUE 5581.
               10  SCB-DESC-005083          PIC X(18) VALUE 'DESC-066079'.
               10  SCB-E-005084             OCCURS 2 TIMES.
                   15  SCB-EK-005084        PIC X(10).
                   15  SCB-EV-005084        PIC 9(06) COMP.
               10  SCB-A-005085             PIC 9(09) COMP VALUE 5085.
               10  SCB-B-005086             PIC S9(07)V99 COMP-3 VALUE +0005086.
               10  SCB-C-005087             PIC X(24) VALUE 'FIELD-005087-ALPHA'.
               10  SCB-D-005088             PIC 9(04) VALUE 5616.
               10  SCB-DESC-005088          PIC X(18) VALUE 'DESC-066144'.
               10  SCB-E-005089             OCCURS 3 TIMES.
                   15  SCB-EK-005089        PIC X(10).
                   15  SCB-EV-005089        PIC 9(06) COMP.
               10  SCB-A-005090             PIC 9(09) COMP VALUE 5090.
               10  SCB-B-005091             PIC S9(07)V99 COMP-3 VALUE +0005091.
           05  SCB-GRP-0464.
               10  SCB-C-005092             PIC X(24) VALUE 'FIELD-005092-ALPHA'.
               10  SCB-D-005093             PIC 9(04) VALUE 5651.
               10  SCB-DESC-005093          PIC X(18) VALUE 'DESC-066209'.
               10  SCB-E-005094             OCCURS 4 TIMES.
                   15  SCB-EK-005094        PIC X(10).
                   15  SCB-EV-005094        PIC 9(06) COMP.
               10  SCB-A-005095             PIC 9(09) COMP VALUE 5095.
               10  SCB-B-005096             PIC S9(07)V99 COMP-3 VALUE +0005096.
               10  SCB-C-005097             PIC X(24) VALUE 'FIELD-005097-ALPHA'.
               10  SCB-D-005098             PIC 9(04) VALUE 5686.
               10  SCB-DESC-005098          PIC X(18) VALUE 'DESC-066274'.
               10  SCB-E-005099             OCCURS 5 TIMES.
                   15  SCB-EK-005099        PIC X(10).
                   15  SCB-EV-005099        PIC 9(06) COMP.
               10  SCB-A-005100             PIC 9(09) COMP VALUE 5100.
               10  SCB-B-005101             PIC S9(07)V99 COMP-3 VALUE +0005101.
           05  SCB-GRP-0465.
               10  SCB-C-005102             PIC X(24) VALUE 'FIELD-005102-ALPHA'.
               10  SCB-D-005103             PIC 9(04) VALUE 5721.
               10  SCB-DESC-005103          PIC X(18) VALUE 'DESC-066339'.
               10  SCB-E-005104             OCCURS 2 TIMES.
                   15  SCB-EK-005104        PIC X(10).
                   15  SCB-EV-005104        PIC 9(06) COMP.
               10  SCB-A-005105             PIC 9(09) COMP VALUE 5105.
               10  SCB-B-005106             PIC S9(07)V99 COMP-3 VALUE +0005106.
               10  SCB-C-005107             PIC X(24) VALUE 'FIELD-005107-ALPHA'.
               10  SCB-D-005108             PIC 9(04) VALUE 5756.
               10  SCB-DESC-005108          PIC X(18) VALUE 'DESC-066404'.
               10  SCB-E-005109             OCCURS 3 TIMES.
                   15  SCB-EK-005109        PIC X(10).
                   15  SCB-EV-005109        PIC 9(06) COMP.
               10  SCB-A-005110             PIC 9(09) COMP VALUE 5110.
               10  SCB-B-005111             PIC S9(07)V99 COMP-3 VALUE +0005111.
               10  SCB-C-005112             PIC X(24) VALUE 'FIELD-005112-ALPHA'.
           05  SCB-GRP-0466.
               10  SCB-D-005113             PIC 9(04) VALUE 5791.
               10  SCB-DESC-005113          PIC X(18) VALUE 'DESC-066469'.
               10  SCB-E-005114             OCCURS 4 TIMES.
                   15  SCB-EK-005114        PIC X(10).
                   15  SCB-EV-005114        PIC 9(06) COMP.
               10  SCB-A-005115             PIC 9(09) COMP VALUE 5115.
               10  SCB-B-005116             PIC S9(07)V99 COMP-3 VALUE +0005116.
               10  SCB-C-005117             PIC X(24) VALUE 'FIELD-005117-ALPHA'.
               10  SCB-D-005118             PIC 9(04) VALUE 5826.
               10  SCB-DESC-005118          PIC X(18) VALUE 'DESC-066534'.
               10  SCB-E-005119             OCCURS 5 TIMES.
                   15  SCB-EK-005119        PIC X(10).
                   15  SCB-EV-005119        PIC 9(06) COMP.
               10  SCB-A-005120             PIC 9(09) COMP VALUE 5120.
               10  SCB-B-005121             PIC S9(07)V99 COMP-3 VALUE +0005121.
               10  SCB-C-005122             PIC X(24) VALUE 'FIELD-005122-ALPHA'.
               10  SCB-D-005123             PIC 9(04) VALUE 5861.
               10  SCB-DESC-005123          PIC X(18) VALUE 'DESC-066599'.
               10  SCB-E-005124             OCCURS 2 TIMES.
                   15  SCB-EK-005124        PIC X(10).
                   15  SCB-EV-005124        PIC 9(06) COMP.
           05  SCB-GRP-0467.
               10  SCB-A-005125             PIC 9(09) COMP VALUE 5125.
               10  SCB-B-005126             PIC S9(07)V99 COMP-3 VALUE +0005126.
               10  SCB-C-005127             PIC X(24) VALUE 'FIELD-005127-ALPHA'.
               10  SCB-D-005128             PIC 9(04) VALUE 5896.
               10  SCB-DESC-005128          PIC X(18) VALUE 'DESC-066664'.
               10  SCB-E-005129             OCCURS 3 TIMES.
                   15  SCB-EK-005129        PIC X(10).
                   15  SCB-EV-005129        PIC 9(06) COMP.
               10  SCB-A-005130             PIC 9(09) COMP VALUE 5130.
               10  SCB-B-005131             PIC S9(07)V99 COMP-3 VALUE +0005131.
               10  SCB-C-005132             PIC X(24) VALUE 'FIELD-005132-ALPHA'.
               10  SCB-D-005133             PIC 9(04) VALUE 5931.
               10  SCB-DESC-005133          PIC X(18) VALUE 'DESC-066729'.
               10  SCB-E-005134             OCCURS 4 TIMES.
                   15  SCB-EK-005134        PIC X(10).
                   15  SCB-EV-005134        PIC 9(06) COMP.
               10  SCB-A-005135             PIC 9(09) COMP VALUE 5135.
               10  SCB-B-005136             PIC S9(07)V99 COMP-3 VALUE +0005136.
               10  SCB-C-005137             PIC X(24) VALUE 'FIELD-005137-ALPHA'.
           05  SCB-GRP-0468.
               10  SCB-D-005138             PIC 9(04) VALUE 5966.
               10  SCB-DESC-005138          PIC X(18) VALUE 'DESC-066794'.
               10  SCB-E-005139             OCCURS 5 TIMES.
                   15  SCB-EK-005139        PIC X(10).
                   15  SCB-EV-005139        PIC 9(06) COMP.
               10  SCB-A-005140             PIC 9(09) COMP VALUE 5140.
               10  SCB-B-005141             PIC S9(07)V99 COMP-3 VALUE +0005141.
               10  SCB-C-005142             PIC X(24) VALUE 'FIELD-005142-ALPHA'.
               10  SCB-D-005143             PIC 9(04) VALUE 6001.
               10  SCB-DESC-005143          PIC X(18) VALUE 'DESC-066859'.
               10  SCB-E-005144             OCCURS 2 TIMES.
                   15  SCB-EK-005144        PIC X(10).
                   15  SCB-EV-005144        PIC 9(06) COMP.
               10  SCB-A-005145             PIC 9(09) COMP VALUE 5145.
               10  SCB-B-005146             PIC S9(07)V99 COMP-3 VALUE +0005146.
               10  SCB-C-005147             PIC X(24) VALUE 'FIELD-005147-ALPHA'.
               10  SCB-D-005148             PIC 9(04) VALUE 6036.
               10  SCB-DESC-005148          PIC X(18) VALUE 'DESC-066924'.
               10  SCB-E-005149             OCCURS 3 TIMES.
                   15  SCB-EK-005149        PIC X(10).
                   15  SCB-EV-005149        PIC 9(06) COMP.
               10  SCB-A-005150             PIC 9(09) COMP VALUE 5150.
               10  SCB-B-005151             PIC S9(07)V99 COMP-3 VALUE +0005151.
           05  SCB-ALT-0078 REDEFINES SCB-GRP-0468.
               10  SCB-ALT-FLAG-0078      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0078   PIC X(64).
           05  SCB-GRP-0469.
               10  SCB-C-005152             PIC X(24) VALUE 'FIELD-005152-ALPHA'.
               10  SCB-D-005153             PIC 9(04) VALUE 6071.
               10  SCB-DESC-005153          PIC X(18) VALUE 'DESC-066989'.
               10  SCB-E-005154             OCCURS 4 TIMES.
                   15  SCB-EK-005154        PIC X(10).
                   15  SCB-EV-005154        PIC 9(06) COMP.
               10  SCB-A-005155             PIC 9(09) COMP VALUE 5155.
               10  SCB-B-005156             PIC S9(07)V99 COMP-3 VALUE +0005156.
               10  SCB-C-005157             PIC X(24) VALUE 'FIELD-005157-ALPHA'.
               10  SCB-D-005158             PIC 9(04) VALUE 6106.
               10  SCB-DESC-005158          PIC X(18) VALUE 'DESC-067054'.
               10  SCB-E-005159             OCCURS 5 TIMES.
                   15  SCB-EK-005159        PIC X(10).
                   15  SCB-EV-005159        PIC 9(06) COMP.
           05  SCB-GRP-0470.
               10  SCB-A-005160             PIC 9(09) COMP VALUE 5160.
               10  SCB-B-005161             PIC S9(07)V99 COMP-3 VALUE +0005161.
               10  SCB-C-005162             PIC X(24) VALUE 'FIELD-005162-ALPHA'.
               10  SCB-D-005163             PIC 9(04) VALUE 6141.
               10  SCB-DESC-005163          PIC X(18) VALUE 'DESC-067119'.
               10  SCB-E-005164             OCCURS 2 TIMES.
                   15  SCB-EK-005164        PIC X(10).
                   15  SCB-EV-005164        PIC 9(06) COMP.
               10  SCB-A-005165             PIC 9(09) COMP VALUE 5165.
               10  SCB-B-005166             PIC S9(07)V99 COMP-3 VALUE +0005166.
               10  SCB-C-005167             PIC X(24) VALUE 'FIELD-005167-ALPHA'.
               10  SCB-D-005168             PIC 9(04) VALUE 6176.
               10  SCB-DESC-005168          PIC X(18) VALUE 'DESC-067184'.
           05  SCB-GRP-0471.
               10  SCB-E-005169             OCCURS 3 TIMES.
                   15  SCB-EK-005169        PIC X(10).
                   15  SCB-EV-005169        PIC 9(06) COMP.
               10  SCB-A-005170             PIC 9(09) COMP VALUE 5170.
               10  SCB-B-005171             PIC S9(07)V99 COMP-3 VALUE +0005171.
               10  SCB-C-005172             PIC X(24) VALUE 'FIELD-005172-ALPHA'.
               10  SCB-D-005173             PIC 9(04) VALUE 6211.
               10  SCB-DESC-005173          PIC X(18) VALUE 'DESC-067249'.
               10  SCB-E-005174             OCCURS 4 TIMES.
                   15  SCB-EK-005174        PIC X(10).
                   15  SCB-EV-005174        PIC 9(06) COMP.
               10  SCB-A-005175             PIC 9(09) COMP VALUE 5175.
               10  SCB-B-005176             PIC S9(07)V99 COMP-3 VALUE +0005176.
               10  SCB-C-005177             PIC X(24) VALUE 'FIELD-005177-ALPHA'.
               10  SCB-D-005178             PIC 9(04) VALUE 6246.
               10  SCB-DESC-005178          PIC X(18) VALUE 'DESC-067314'.
           05  SCB-GRP-0472.
               10  SCB-E-005179             OCCURS 5 TIMES.
                   15  SCB-EK-005179        PIC X(10).
                   15  SCB-EV-005179        PIC 9(06) COMP.
               10  SCB-A-005180             PIC 9(09) COMP VALUE 5180.
               10  SCB-B-005181             PIC S9(07)V99 COMP-3 VALUE +0005181.
               10  SCB-C-005182             PIC X(24) VALUE 'FIELD-005182-ALPHA'.
               10  SCB-D-005183             PIC 9(04) VALUE 6281.
               10  SCB-DESC-005183          PIC X(18) VALUE 'DESC-067379'.
               10  SCB-E-005184             OCCURS 2 TIMES.
                   15  SCB-EK-005184        PIC X(10).
                   15  SCB-EV-005184        PIC 9(06) COMP.
               10  SCB-A-005185             PIC 9(09) COMP VALUE 5185.
               10  SCB-B-005186             PIC S9(07)V99 COMP-3 VALUE +0005186.
               10  SCB-C-005187             PIC X(24) VALUE 'FIELD-005187-ALPHA'.
               10  SCB-D-005188             PIC 9(04) VALUE 6316.
               10  SCB-DESC-005188          PIC X(18) VALUE 'DESC-067444'.
               10  SCB-E-005189             OCCURS 3 TIMES.
                   15  SCB-EK-005189        PIC X(10).
                   15  SCB-EV-005189        PIC 9(06) COMP.
           05  SCB-GRP-0473.
               10  SCB-A-005190             PIC 9(09) COMP VALUE 5190.
               10  SCB-B-005191             PIC S9(07)V99 COMP-3 VALUE +0005191.
               10  SCB-C-005192             PIC X(24) VALUE 'FIELD-005192-ALPHA'.
               10  SCB-D-005193             PIC 9(04) VALUE 6351.
               10  SCB-DESC-005193          PIC X(18) VALUE 'DESC-067509'.
               10  SCB-E-005194             OCCURS 4 TIMES.
                   15  SCB-EK-005194        PIC X(10).
                   15  SCB-EV-005194        PIC 9(06) COMP.
               10  SCB-A-005195             PIC 9(09) COMP VALUE 5195.
               10  SCB-B-005196             PIC S9(07)V99 COMP-3 VALUE +0005196.
               10  SCB-C-005197             PIC X(24) VALUE 'FIELD-005197-ALPHA'.
               10  SCB-D-005198             PIC 9(04) VALUE 6386.
               10  SCB-DESC-005198          PIC X(18) VALUE 'DESC-067574'.
               10  SCB-E-005199             OCCURS 5 TIMES.
                   15  SCB-EK-005199        PIC X(10).
                   15  SCB-EV-005199        PIC 9(06) COMP.
               10  SCB-A-005200             PIC 9(09) COMP VALUE 5200.
               10  SCB-B-005201             PIC S9(07)V99 COMP-3 VALUE +0005201.
           05  SCB-GRP-0474.
               10  SCB-C-005202             PIC X(24) VALUE 'FIELD-005202-ALPHA'.
               10  SCB-D-005203             PIC 9(04) VALUE 6421.
               10  SCB-DESC-005203          PIC X(18) VALUE 'DESC-067639'.
               10  SCB-E-005204             OCCURS 2 TIMES.
                   15  SCB-EK-005204        PIC X(10).
                   15  SCB-EV-005204        PIC 9(06) COMP.
               10  SCB-A-005205             PIC 9(09) COMP VALUE 5205.
               10  SCB-B-005206             PIC S9(07)V99 COMP-3 VALUE +0005206.
               10  SCB-C-005207             PIC X(24) VALUE 'FIELD-005207-ALPHA'.
               10  SCB-D-005208             PIC 9(04) VALUE 6456.
               10  SCB-DESC-005208          PIC X(18) VALUE 'DESC-067704'.
               10  SCB-E-005209             OCCURS 3 TIMES.
                   15  SCB-EK-005209        PIC X(10).
                   15  SCB-EV-005209        PIC 9(06) COMP.
               10  SCB-A-005210             PIC 9(09) COMP VALUE 5210.
               10  SCB-B-005211             PIC S9(07)V99 COMP-3 VALUE +0005211.
               10  SCB-C-005212             PIC X(24) VALUE 'FIELD-005212-ALPHA'.
               10  SCB-D-005213             PIC 9(04) VALUE 6491.
               10  SCB-DESC-005213          PIC X(18) VALUE 'DESC-067769'.
               10  SCB-E-005214             OCCURS 4 TIMES.
                   15  SCB-EK-005214        PIC X(10).
                   15  SCB-EV-005214        PIC 9(06) COMP.
           05  SCB-ALT-0079 REDEFINES SCB-GRP-0474.
               10  SCB-ALT-FLAG-0079      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0079   PIC X(64).
           05  SCB-GRP-0475.
               10  SCB-A-005215             PIC 9(09) COMP VALUE 5215.
               10  SCB-B-005216             PIC S9(07)V99 COMP-3 VALUE +0005216.
               10  SCB-C-005217             PIC X(24) VALUE 'FIELD-005217-ALPHA'.
               10  SCB-D-005218             PIC 9(04) VALUE 6526.
               10  SCB-DESC-005218          PIC X(18) VALUE 'DESC-067834'.
               10  SCB-E-005219             OCCURS 5 TIMES.
                   15  SCB-EK-005219        PIC X(10).
                   15  SCB-EV-005219        PIC 9(06) COMP.
               10  SCB-A-005220             PIC 9(09) COMP VALUE 5220.
               10  SCB-B-005221             PIC S9(07)V99 COMP-3 VALUE +0005221.
               10  SCB-C-005222             PIC X(24) VALUE 'FIELD-005222-ALPHA'.
               10  SCB-D-005223             PIC 9(04) VALUE 6561.
               10  SCB-DESC-005223          PIC X(18) VALUE 'DESC-067899'.
               10  SCB-E-005224             OCCURS 2 TIMES.
                   15  SCB-EK-005224        PIC X(10).
                   15  SCB-EV-005224        PIC 9(06) COMP.
               10  SCB-A-005225             PIC 9(09) COMP VALUE 5225.
               10  SCB-B-005226             PIC S9(07)V99 COMP-3 VALUE +0005226.
               10  SCB-C-005227             PIC X(24) VALUE 'FIELD-005227-ALPHA'.
               10  SCB-D-005228             PIC 9(04) VALUE 6596.
               10  SCB-DESC-005228          PIC X(18) VALUE 'DESC-067964'.
           05  SCB-GRP-0476.
               10  SCB-E-005229             OCCURS 3 TIMES.
                   15  SCB-EK-005229        PIC X(10).
                   15  SCB-EV-005229        PIC 9(06) COMP.
               10  SCB-A-005230             PIC 9(09) COMP VALUE 5230.
               10  SCB-B-005231             PIC S9(07)V99 COMP-3 VALUE +0005231.
               10  SCB-C-005232             PIC X(24) VALUE 'FIELD-005232-ALPHA'.
               10  SCB-D-005233             PIC 9(04) VALUE 6631.
               10  SCB-DESC-005233          PIC X(18) VALUE 'DESC-068029'.
               10  SCB-E-005234             OCCURS 4 TIMES.
                   15  SCB-EK-005234        PIC X(10).
                   15  SCB-EV-005234        PIC 9(06) COMP.
               10  SCB-A-005235             PIC 9(09) COMP VALUE 5235.
               10  SCB-B-005236             PIC S9(07)V99 COMP-3 VALUE +0005236.
           05  SCB-GRP-0477.
               10  SCB-C-005237             PIC X(24) VALUE 'FIELD-005237-ALPHA'.
               10  SCB-D-005238             PIC 9(04) VALUE 6666.
               10  SCB-DESC-005238          PIC X(18) VALUE 'DESC-068094'.
               10  SCB-E-005239             OCCURS 5 TIMES.
                   15  SCB-EK-005239        PIC X(10).
                   15  SCB-EV-005239        PIC 9(06) COMP.
               10  SCB-A-005240             PIC 9(09) COMP VALUE 5240.
               10  SCB-B-005241             PIC S9(07)V99 COMP-3 VALUE +0005241.
               10  SCB-C-005242             PIC X(24) VALUE 'FIELD-005242-ALPHA'.
               10  SCB-D-005243             PIC 9(04) VALUE 6701.
               10  SCB-DESC-005243          PIC X(18) VALUE 'DESC-068159'.
               10  SCB-E-005244             OCCURS 2 TIMES.
                   15  SCB-EK-005244        PIC X(10).
                   15  SCB-EV-005244        PIC 9(06) COMP.
               10  SCB-A-005245             PIC 9(09) COMP VALUE 5245.
           05  SCB-GRP-0478.
               10  SCB-B-005246             PIC S9(07)V99 COMP-3 VALUE +0005246.
               10  SCB-C-005247             PIC X(24) VALUE 'FIELD-005247-ALPHA'.
               10  SCB-D-005248             PIC 9(04) VALUE 6736.
               10  SCB-DESC-005248          PIC X(18) VALUE 'DESC-068224'.
               10  SCB-E-005249             OCCURS 3 TIMES.
                   15  SCB-EK-005249        PIC X(10).
                   15  SCB-EV-005249        PIC 9(06) COMP.
               10  SCB-A-005250             PIC 9(09) COMP VALUE 5250.
               10  SCB-B-005251             PIC S9(07)V99 COMP-3 VALUE +0005251.
               10  SCB-C-005252             PIC X(24) VALUE 'FIELD-005252-ALPHA'.
               10  SCB-D-005253             PIC 9(04) VALUE 6771.
               10  SCB-DESC-005253          PIC X(18) VALUE 'DESC-068289'.
               10  SCB-E-005254             OCCURS 4 TIMES.
                   15  SCB-EK-005254        PIC X(10).
                   15  SCB-EV-005254        PIC 9(06) COMP.
               10  SCB-A-005255             PIC 9(09) COMP VALUE 5255.
           05  SCB-GRP-0479.
               10  SCB-B-005256             PIC S9(07)V99 COMP-3 VALUE +0005256.
               10  SCB-C-005257             PIC X(24) VALUE 'FIELD-005257-ALPHA'.
               10  SCB-D-005258             PIC 9(04) VALUE 6806.
               10  SCB-DESC-005258          PIC X(18) VALUE 'DESC-068354'.
               10  SCB-E-005259             OCCURS 5 TIMES.
                   15  SCB-EK-005259        PIC X(10).
                   15  SCB-EV-005259        PIC 9(06) COMP.
               10  SCB-A-005260             PIC 9(09) COMP VALUE 5260.
               10  SCB-B-005261             PIC S9(07)V99 COMP-3 VALUE +0005261.
               10  SCB-C-005262             PIC X(24) VALUE 'FIELD-005262-ALPHA'.
               10  SCB-D-005263             PIC 9(04) VALUE 6841.
               10  SCB-DESC-005263          PIC X(18) VALUE 'DESC-068419'.
               10  SCB-E-005264             OCCURS 2 TIMES.
                   15  SCB-EK-005264        PIC X(10).
                   15  SCB-EV-005264        PIC 9(06) COMP.
               10  SCB-A-005265             PIC 9(09) COMP VALUE 5265.
               10  SCB-B-005266             PIC S9(07)V99 COMP-3 VALUE +0005266.
           05  SCB-GRP-0480.
               10  SCB-C-005267             PIC X(24) VALUE 'FIELD-005267-ALPHA'.
               10  SCB-D-005268             PIC 9(04) VALUE 6876.
               10  SCB-DESC-005268          PIC X(18) VALUE 'DESC-068484'.
               10  SCB-E-005269             OCCURS 3 TIMES.
                   15  SCB-EK-005269        PIC X(10).
                   15  SCB-EV-005269        PIC 9(06) COMP.
               10  SCB-A-005270             PIC 9(09) COMP VALUE 5270.
               10  SCB-B-005271             PIC S9(07)V99 COMP-3 VALUE +0005271.
               10  SCB-C-005272             PIC X(24) VALUE 'FIELD-005272-ALPHA'.
               10  SCB-D-005273             PIC 9(04) VALUE 6911.
               10  SCB-DESC-005273          PIC X(18) VALUE 'DESC-068549'.
               10  SCB-E-005274             OCCURS 4 TIMES.
                   15  SCB-EK-005274        PIC X(10).
                   15  SCB-EV-005274        PIC 9(06) COMP.
               10  SCB-A-005275             PIC 9(09) COMP VALUE 5275.
               10  SCB-B-005276             PIC S9(07)V99 COMP-3 VALUE +0005276.
               10  SCB-C-005277             PIC X(24) VALUE 'FIELD-005277-ALPHA'.
               10  SCB-D-005278             PIC 9(04) VALUE 6946.
               10  SCB-DESC-005278          PIC X(18) VALUE 'DESC-068614'.
           05  SCB-ALT-0080 REDEFINES SCB-GRP-0480.
               10  SCB-ALT-FLAG-0080      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0080   PIC X(64).
           05  SCB-GRP-0481.
               10  SCB-E-005279             OCCURS 5 TIMES.
                   15  SCB-EK-005279        PIC X(10).
                   15  SCB-EV-005279        PIC 9(06) COMP.
               10  SCB-A-005280             PIC 9(09) COMP VALUE 5280.
               10  SCB-B-005281             PIC S9(07)V99 COMP-3 VALUE +0005281.
               10  SCB-C-005282             PIC X(24) VALUE 'FIELD-005282-ALPHA'.
               10  SCB-D-005283             PIC 9(04) VALUE 6981.
               10  SCB-DESC-005283          PIC X(18) VALUE 'DESC-068679'.
               10  SCB-E-005284             OCCURS 2 TIMES.
                   15  SCB-EK-005284        PIC X(10).
                   15  SCB-EV-005284        PIC 9(06) COMP.
               10  SCB-A-005285             PIC 9(09) COMP VALUE 5285.
               10  SCB-B-005286             PIC S9(07)V99 COMP-3 VALUE +0005286.
               10  SCB-C-005287             PIC X(24) VALUE 'FIELD-005287-ALPHA'.
               10  SCB-D-005288             PIC 9(04) VALUE 7016.
               10  SCB-DESC-005288          PIC X(18) VALUE 'DESC-068744'.
               10  SCB-E-005289             OCCURS 3 TIMES.
                   15  SCB-EK-005289        PIC X(10).
                   15  SCB-EV-005289        PIC 9(06) COMP.
               10  SCB-A-005290             PIC 9(09) COMP VALUE 5290.
               10  SCB-B-005291             PIC S9(07)V99 COMP-3 VALUE +0005291.
           05  SCB-GRP-0482.
               10  SCB-C-005292             PIC X(24) VALUE 'FIELD-005292-ALPHA'.
               10  SCB-D-005293             PIC 9(04) VALUE 7051.
               10  SCB-DESC-005293          PIC X(18) VALUE 'DESC-068809'.
               10  SCB-E-005294             OCCURS 4 TIMES.
                   15  SCB-EK-005294        PIC X(10).
                   15  SCB-EV-005294        PIC 9(06) COMP.
               10  SCB-A-005295             PIC 9(09) COMP VALUE 5295.
               10  SCB-B-005296             PIC S9(07)V99 COMP-3 VALUE +0005296.
               10  SCB-C-005297             PIC X(24) VALUE 'FIELD-005297-ALPHA'.
               10  SCB-D-005298             PIC 9(04) VALUE 7086.
               10  SCB-DESC-005298          PIC X(18) VALUE 'DESC-068874'.
               10  SCB-E-005299             OCCURS 5 TIMES.
                   15  SCB-EK-005299        PIC X(10).
                   15  SCB-EV-005299        PIC 9(06) COMP.
               10  SCB-A-005300             PIC 9(09) COMP VALUE 5300.
               10  SCB-B-005301             PIC S9(07)V99 COMP-3 VALUE +0005301.
               10  SCB-C-005302             PIC X(24) VALUE 'FIELD-005302-ALPHA'.
               10  SCB-D-005303             PIC 9(04) VALUE 7121.
               10  SCB-DESC-005303          PIC X(18) VALUE 'DESC-068939'.
               10  SCB-E-005304             OCCURS 2 TIMES.
                   15  SCB-EK-005304        PIC X(10).
                   15  SCB-EV-005304        PIC 9(06) COMP.
               10  SCB-A-005305             PIC 9(09) COMP VALUE 5305.
           05  SCB-GRP-0483.
               10  SCB-B-005306             PIC S9(07)V99 COMP-3 VALUE +0005306.
               10  SCB-C-005307             PIC X(24) VALUE 'FIELD-005307-ALPHA'.
               10  SCB-D-005308             PIC 9(04) VALUE 7156.
               10  SCB-DESC-005308          PIC X(18) VALUE 'DESC-069004'.
               10  SCB-E-005309             OCCURS 3 TIMES.
                   15  SCB-EK-005309        PIC X(10).
                   15  SCB-EV-005309        PIC 9(06) COMP.
               10  SCB-A-005310             PIC 9(09) COMP VALUE 5310.
               10  SCB-B-005311             PIC S9(07)V99 COMP-3 VALUE +0005311.
               10  SCB-C-005312             PIC X(24) VALUE 'FIELD-005312-ALPHA'.
               10  SCB-D-005313             PIC 9(04) VALUE 7191.
               10  SCB-DESC-005313          PIC X(18) VALUE 'DESC-069069'.
           05  SCB-GRP-0484.
               10  SCB-E-005314             OCCURS 4 TIMES.
                   15  SCB-EK-005314        PIC X(10).
                   15  SCB-EV-005314        PIC 9(06) COMP.
               10  SCB-A-005315             PIC 9(09) COMP VALUE 5315.
               10  SCB-B-005316             PIC S9(07)V99 COMP-3 VALUE +0005316.
               10  SCB-C-005317             PIC X(24) VALUE 'FIELD-005317-ALPHA'.
               10  SCB-D-005318             PIC 9(04) VALUE 7226.
               10  SCB-DESC-005318          PIC X(18) VALUE 'DESC-069134'.
               10  SCB-E-005319             OCCURS 5 TIMES.
                   15  SCB-EK-005319        PIC X(10).
                   15  SCB-EV-005319        PIC 9(06) COMP.
               10  SCB-A-005320             PIC 9(09) COMP VALUE 5320.
               10  SCB-B-005321             PIC S9(07)V99 COMP-3 VALUE +0005321.
               10  SCB-C-005322             PIC X(24) VALUE 'FIELD-005322-ALPHA'.
           05  SCB-GRP-0485.
               10  SCB-D-005323             PIC 9(04) VALUE 7261.
               10  SCB-DESC-005323          PIC X(18) VALUE 'DESC-069199'.
               10  SCB-E-005324             OCCURS 2 TIMES.
                   15  SCB-EK-005324        PIC X(10).
                   15  SCB-EV-005324        PIC 9(06) COMP.
               10  SCB-A-005325             PIC 9(09) COMP VALUE 5325.
               10  SCB-B-005326             PIC S9(07)V99 COMP-3 VALUE +0005326.
               10  SCB-C-005327             PIC X(24) VALUE 'FIELD-005327-ALPHA'.
               10  SCB-D-005328             PIC 9(04) VALUE 7296.
               10  SCB-DESC-005328          PIC X(18) VALUE 'DESC-069264'.
               10  SCB-E-005329             OCCURS 3 TIMES.
                   15  SCB-EK-005329        PIC X(10).
                   15  SCB-EV-005329        PIC 9(06) COMP.
               10  SCB-A-005330             PIC 9(09) COMP VALUE 5330.
               10  SCB-B-005331             PIC S9(07)V99 COMP-3 VALUE +0005331.
               10  SCB-C-005332             PIC X(24) VALUE 'FIELD-005332-ALPHA'.
           05  SCB-GRP-0486.
               10  SCB-D-005333             PIC 9(04) VALUE 7331.
               10  SCB-DESC-005333          PIC X(18) VALUE 'DESC-069329'.
               10  SCB-E-005334             OCCURS 4 TIMES.
                   15  SCB-EK-005334        PIC X(10).
                   15  SCB-EV-005334        PIC 9(06) COMP.
               10  SCB-A-005335             PIC 9(09) COMP VALUE 5335.
               10  SCB-B-005336             PIC S9(07)V99 COMP-3 VALUE +0005336.
               10  SCB-C-005337             PIC X(24) VALUE 'FIELD-005337-ALPHA'.
               10  SCB-D-005338             PIC 9(04) VALUE 7366.
               10  SCB-DESC-005338          PIC X(18) VALUE 'DESC-069394'.
               10  SCB-E-005339             OCCURS 5 TIMES.
                   15  SCB-EK-005339        PIC X(10).
                   15  SCB-EV-005339        PIC 9(06) COMP.
               10  SCB-A-005340             PIC 9(09) COMP VALUE 5340.
               10  SCB-B-005341             PIC S9(07)V99 COMP-3 VALUE +0005341.
               10  SCB-C-005342             PIC X(24) VALUE 'FIELD-005342-ALPHA'.
               10  SCB-D-005343             PIC 9(04) VALUE 7401.
               10  SCB-DESC-005343          PIC X(18) VALUE 'DESC-069459'.
           05  SCB-ALT-0081 REDEFINES SCB-GRP-0486.
               10  SCB-ALT-FLAG-0081      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0081   PIC X(64).
           05  SCB-GRP-0487.
               10  SCB-E-005344             OCCURS 2 TIMES.
                   15  SCB-EK-005344        PIC X(10).
                   15  SCB-EV-005344        PIC 9(06) COMP.
               10  SCB-A-005345             PIC 9(09) COMP VALUE 5345.
               10  SCB-B-005346             PIC S9(07)V99 COMP-3 VALUE +0005346.
               10  SCB-C-005347             PIC X(24) VALUE 'FIELD-005347-ALPHA'.
               10  SCB-D-005348             PIC 9(04) VALUE 7436.
               10  SCB-DESC-005348          PIC X(18) VALUE 'DESC-069524'.
               10  SCB-E-005349             OCCURS 3 TIMES.
                   15  SCB-EK-005349        PIC X(10).
                   15  SCB-EV-005349        PIC 9(06) COMP.
               10  SCB-A-005350             PIC 9(09) COMP VALUE 5350.
               10  SCB-B-005351             PIC S9(07)V99 COMP-3 VALUE +0005351.
               10  SCB-C-005352             PIC X(24) VALUE 'FIELD-005352-ALPHA'.
               10  SCB-D-005353             PIC 9(04) VALUE 7471.
               10  SCB-DESC-005353          PIC X(18) VALUE 'DESC-069589'.
               10  SCB-E-005354             OCCURS 4 TIMES.
                   15  SCB-EK-005354        PIC X(10).
                   15  SCB-EV-005354        PIC 9(06) COMP.
               10  SCB-A-005355             PIC 9(09) COMP VALUE 5355.
           05  SCB-GRP-0488.
               10  SCB-B-005356             PIC S9(07)V99 COMP-3 VALUE +0005356.
               10  SCB-C-005357             PIC X(24) VALUE 'FIELD-005357-ALPHA'.
               10  SCB-D-005358             PIC 9(04) VALUE 7506.
               10  SCB-DESC-005358          PIC X(18) VALUE 'DESC-069654'.
               10  SCB-E-005359             OCCURS 5 TIMES.
                   15  SCB-EK-005359        PIC X(10).
                   15  SCB-EV-005359        PIC 9(06) COMP.
               10  SCB-A-005360             PIC 9(09) COMP VALUE 5360.
               10  SCB-B-005361             PIC S9(07)V99 COMP-3 VALUE +0005361.
               10  SCB-C-005362             PIC X(24) VALUE 'FIELD-005362-ALPHA'.
               10  SCB-D-005363             PIC 9(04) VALUE 7541.
               10  SCB-DESC-005363          PIC X(18) VALUE 'DESC-069719'.
               10  SCB-E-005364             OCCURS 2 TIMES.
                   15  SCB-EK-005364        PIC X(10).
                   15  SCB-EV-005364        PIC 9(06) COMP.
               10  SCB-A-005365             PIC 9(09) COMP VALUE 5365.
               10  SCB-B-005366             PIC S9(07)V99 COMP-3 VALUE +0005366.
               10  SCB-C-005367             PIC X(24) VALUE 'FIELD-005367-ALPHA'.
               10  SCB-D-005368             PIC 9(04) VALUE 7576.
               10  SCB-DESC-005368          PIC X(18) VALUE 'DESC-069784'.
           05  SCB-GRP-0489.
               10  SCB-E-005369             OCCURS 3 TIMES.
                   15  SCB-EK-005369        PIC X(10).
                   15  SCB-EV-005369        PIC 9(06) COMP.
               10  SCB-A-005370             PIC 9(09) COMP VALUE 5370.
               10  SCB-B-005371             PIC S9(07)V99 COMP-3 VALUE +0005371.
               10  SCB-C-005372             PIC X(24) VALUE 'FIELD-005372-ALPHA'.
               10  SCB-D-005373             PIC 9(04) VALUE 7611.
               10  SCB-DESC-005373          PIC X(18) VALUE 'DESC-069849'.
               10  SCB-E-005374             OCCURS 4 TIMES.
                   15  SCB-EK-005374        PIC X(10).
                   15  SCB-EV-005374        PIC 9(06) COMP.
               10  SCB-A-005375             PIC 9(09) COMP VALUE 5375.
               10  SCB-B-005376             PIC S9(07)V99 COMP-3 VALUE +0005376.
               10  SCB-C-005377             PIC X(24) VALUE 'FIELD-005377-ALPHA'.
               10  SCB-D-005378             PIC 9(04) VALUE 7646.
               10  SCB-DESC-005378          PIC X(18) VALUE 'DESC-069914'.
               10  SCB-E-005379             OCCURS 5 TIMES.
                   15  SCB-EK-005379        PIC X(10).
                   15  SCB-EV-005379        PIC 9(06) COMP.
               10  SCB-A-005380             PIC 9(09) COMP VALUE 5380.
               10  SCB-B-005381             PIC S9(07)V99 COMP-3 VALUE +0005381.
               10  SCB-C-005382             PIC X(24) VALUE 'FIELD-005382-ALPHA'.
           05  SCB-GRP-0490.
               10  SCB-D-005383             PIC 9(04) VALUE 7681.
               10  SCB-DESC-005383          PIC X(18) VALUE 'DESC-069979'.
               10  SCB-E-005384             OCCURS 2 TIMES.
                   15  SCB-EK-005384        PIC X(10).
                   15  SCB-EV-005384        PIC 9(06) COMP.
               10  SCB-A-005385             PIC 9(09) COMP VALUE 5385.
               10  SCB-B-005386             PIC S9(07)V99 COMP-3 VALUE +0005386.
               10  SCB-C-005387             PIC X(24) VALUE 'FIELD-005387-ALPHA'.
               10  SCB-D-005388             PIC 9(04) VALUE 7716.
               10  SCB-DESC-005388          PIC X(18) VALUE 'DESC-070044'.
               10  SCB-E-005389             OCCURS 3 TIMES.
                   15  SCB-EK-005389        PIC X(10).
                   15  SCB-EV-005389        PIC 9(06) COMP.
               10  SCB-A-005390             PIC 9(09) COMP VALUE 5390.
           05  SCB-GRP-0491.
               10  SCB-B-005391             PIC S9(07)V99 COMP-3 VALUE +0005391.
               10  SCB-C-005392             PIC X(24) VALUE 'FIELD-005392-ALPHA'.
               10  SCB-D-005393             PIC 9(04) VALUE 7751.
               10  SCB-DESC-005393          PIC X(18) VALUE 'DESC-070109'.
               10  SCB-E-005394             OCCURS 4 TIMES.
                   15  SCB-EK-005394        PIC X(10).
                   15  SCB-EV-005394        PIC 9(06) COMP.
               10  SCB-A-005395             PIC 9(09) COMP VALUE 5395.
               10  SCB-B-005396             PIC S9(07)V99 COMP-3 VALUE +0005396.
               10  SCB-C-005397             PIC X(24) VALUE 'FIELD-005397-ALPHA'.
               10  SCB-D-005398             PIC 9(04) VALUE 7786.
               10  SCB-DESC-005398          PIC X(18) VALUE 'DESC-070174'.
               10  SCB-E-005399             OCCURS 5 TIMES.
                   15  SCB-EK-005399        PIC X(10).
                   15  SCB-EV-005399        PIC 9(06) COMP.
           05  SCB-GRP-0492.
               10  SCB-A-005400             PIC 9(09) COMP VALUE 5400.
               10  SCB-B-005401             PIC S9(07)V99 COMP-3 VALUE +0005401.
               10  SCB-C-005402             PIC X(24) VALUE 'FIELD-005402-ALPHA'.
               10  SCB-D-005403             PIC 9(04) VALUE 7821.
               10  SCB-DESC-005403          PIC X(18) VALUE 'DESC-070239'.
               10  SCB-E-005404             OCCURS 2 TIMES.
                   15  SCB-EK-005404        PIC X(10).
                   15  SCB-EV-005404        PIC 9(06) COMP.
               10  SCB-A-005405             PIC 9(09) COMP VALUE 5405.
               10  SCB-B-005406             PIC S9(07)V99 COMP-3 VALUE +0005406.
               10  SCB-C-005407             PIC X(24) VALUE 'FIELD-005407-ALPHA'.
               10  SCB-D-005408             PIC 9(04) VALUE 7856.
               10  SCB-DESC-005408          PIC X(18) VALUE 'DESC-070304'.
               10  SCB-E-005409             OCCURS 3 TIMES.
                   15  SCB-EK-005409        PIC X(10).
                   15  SCB-EV-005409        PIC 9(06) COMP.
           05  SCB-ALT-0082 REDEFINES SCB-GRP-0492.
               10  SCB-ALT-FLAG-0082      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0082   PIC X(64).
           05  SCB-GRP-0493.
               10  SCB-A-005410             PIC 9(09) COMP VALUE 5410.
               10  SCB-B-005411             PIC S9(07)V99 COMP-3 VALUE +0005411.
               10  SCB-C-005412             PIC X(24) VALUE 'FIELD-005412-ALPHA'.
               10  SCB-D-005413             PIC 9(04) VALUE 7891.
               10  SCB-DESC-005413          PIC X(18) VALUE 'DESC-070369'.
               10  SCB-E-005414             OCCURS 4 TIMES.
                   15  SCB-EK-005414        PIC X(10).
                   15  SCB-EV-005414        PIC 9(06) COMP.
               10  SCB-A-005415             PIC 9(09) COMP VALUE 5415.
               10  SCB-B-005416             PIC S9(07)V99 COMP-3 VALUE +0005416.
               10  SCB-C-005417             PIC X(24) VALUE 'FIELD-005417-ALPHA'.
               10  SCB-D-005418             PIC 9(04) VALUE 7926.
               10  SCB-DESC-005418          PIC X(18) VALUE 'DESC-070434'.
               10  SCB-E-005419             OCCURS 5 TIMES.
                   15  SCB-EK-005419        PIC X(10).
                   15  SCB-EV-005419        PIC 9(06) COMP.
               10  SCB-A-005420             PIC 9(09) COMP VALUE 5420.
           05  SCB-GRP-0494.
               10  SCB-B-005421             PIC S9(07)V99 COMP-3 VALUE +0005421.
               10  SCB-C-005422             PIC X(24) VALUE 'FIELD-005422-ALPHA'.
               10  SCB-D-005423             PIC 9(04) VALUE 7961.
               10  SCB-DESC-005423          PIC X(18) VALUE 'DESC-070499'.
               10  SCB-E-005424             OCCURS 2 TIMES.
                   15  SCB-EK-005424        PIC X(10).
                   15  SCB-EV-005424        PIC 9(06) COMP.
               10  SCB-A-005425             PIC 9(09) COMP VALUE 5425.
               10  SCB-B-005426             PIC S9(07)V99 COMP-3 VALUE +0005426.
               10  SCB-C-005427             PIC X(24) VALUE 'FIELD-005427-ALPHA'.
               10  SCB-D-005428             PIC 9(04) VALUE 7996.
               10  SCB-DESC-005428          PIC X(18) VALUE 'DESC-070564'.
               10  SCB-E-005429             OCCURS 3 TIMES.
                   15  SCB-EK-005429        PIC X(10).
                   15  SCB-EV-005429        PIC 9(06) COMP.
               10  SCB-A-005430             PIC 9(09) COMP VALUE 5430.
               10  SCB-B-005431             PIC S9(07)V99 COMP-3 VALUE +0005431.
               10  SCB-C-005432             PIC X(24) VALUE 'FIELD-005432-ALPHA'.
           05  SCB-GRP-0495.
               10  SCB-D-005433             PIC 9(04) VALUE 8031.
               10  SCB-DESC-005433          PIC X(18) VALUE 'DESC-070629'.
               10  SCB-E-005434             OCCURS 4 TIMES.
                   15  SCB-EK-005434        PIC X(10).
                   15  SCB-EV-005434        PIC 9(06) COMP.
               10  SCB-A-005435             PIC 9(09) COMP VALUE 5435.
               10  SCB-B-005436             PIC S9(07)V99 COMP-3 VALUE +0005436.
               10  SCB-C-005437             PIC X(24) VALUE 'FIELD-005437-ALPHA'.
               10  SCB-D-005438             PIC 9(04) VALUE 8066.
               10  SCB-DESC-005438          PIC X(18) VALUE 'DESC-070694'.
               10  SCB-E-005439             OCCURS 5 TIMES.
                   15  SCB-EK-005439        PIC X(10).
                   15  SCB-EV-005439        PIC 9(06) COMP.
               10  SCB-A-005440             PIC 9(09) COMP VALUE 5440.
               10  SCB-B-005441             PIC S9(07)V99 COMP-3 VALUE +0005441.
               10  SCB-C-005442             PIC X(24) VALUE 'FIELD-005442-ALPHA'.
               10  SCB-D-005443             PIC 9(04) VALUE 8101.
               10  SCB-DESC-005443          PIC X(18) VALUE 'DESC-070759'.
               10  SCB-E-005444             OCCURS 2 TIMES.
                   15  SCB-EK-005444        PIC X(10).
                   15  SCB-EV-005444        PIC 9(06) COMP.
               10  SCB-A-005445             PIC 9(09) COMP VALUE 5445.
           05  SCB-GRP-0496.
               10  SCB-B-005446             PIC S9(07)V99 COMP-3 VALUE +0005446.
               10  SCB-C-005447             PIC X(24) VALUE 'FIELD-005447-ALPHA'.
               10  SCB-D-005448             PIC 9(04) VALUE 8136.
               10  SCB-DESC-005448          PIC X(18) VALUE 'DESC-070824'.
               10  SCB-E-005449             OCCURS 3 TIMES.
                   15  SCB-EK-005449        PIC X(10).
                   15  SCB-EV-005449        PIC 9(06) COMP.
               10  SCB-A-005450             PIC 9(09) COMP VALUE 5450.
               10  SCB-B-005451             PIC S9(07)V99 COMP-3 VALUE +0005451.
               10  SCB-C-005452             PIC X(24) VALUE 'FIELD-005452-ALPHA'.
               10  SCB-D-005453             PIC 9(04) VALUE 8171.
               10  SCB-DESC-005453          PIC X(18) VALUE 'DESC-070889'.
               10  SCB-E-005454             OCCURS 4 TIMES.
                   15  SCB-EK-005454        PIC X(10).
                   15  SCB-EV-005454        PIC 9(06) COMP.
               10  SCB-A-005455             PIC 9(09) COMP VALUE 5455.
               10  SCB-B-005456             PIC S9(07)V99 COMP-3 VALUE +0005456.
               10  SCB-C-005457             PIC X(24) VALUE 'FIELD-005457-ALPHA'.
               10  SCB-D-005458             PIC 9(04) VALUE 8206.
               10  SCB-DESC-005458          PIC X(18) VALUE 'DESC-070954'.
               10  SCB-E-005459             OCCURS 5 TIMES.
                   15  SCB-EK-005459        PIC X(10).
                   15  SCB-EV-005459        PIC 9(06) COMP.
           05  SCB-GRP-0497.
               10  SCB-A-005460             PIC 9(09) COMP VALUE 5460.
               10  SCB-B-005461             PIC S9(07)V99 COMP-3 VALUE +0005461.
               10  SCB-C-005462             PIC X(24) VALUE 'FIELD-005462-ALPHA'.
               10  SCB-D-005463             PIC 9(04) VALUE 8241.
               10  SCB-DESC-005463          PIC X(18) VALUE 'DESC-071019'.
               10  SCB-E-005464             OCCURS 2 TIMES.
                   15  SCB-EK-005464        PIC X(10).
                   15  SCB-EV-005464        PIC 9(06) COMP.
               10  SCB-A-005465             PIC 9(09) COMP VALUE 5465.
               10  SCB-B-005466             PIC S9(07)V99 COMP-3 VALUE +0005466.
               10  SCB-C-005467             PIC X(24) VALUE 'FIELD-005467-ALPHA'.
           05  SCB-GRP-0498.
               10  SCB-D-005468             PIC 9(04) VALUE 8276.
               10  SCB-DESC-005468          PIC X(18) VALUE 'DESC-071084'.
               10  SCB-E-005469             OCCURS 3 TIMES.
                   15  SCB-EK-005469        PIC X(10).
                   15  SCB-EV-005469        PIC 9(06) COMP.
               10  SCB-A-005470             PIC 9(09) COMP VALUE 5470.
               10  SCB-B-005471             PIC S9(07)V99 COMP-3 VALUE +0005471.
               10  SCB-C-005472             PIC X(24) VALUE 'FIELD-005472-ALPHA'.
               10  SCB-D-005473             PIC 9(04) VALUE 8311.
               10  SCB-DESC-005473          PIC X(18) VALUE 'DESC-071149'.
               10  SCB-E-005474             OCCURS 4 TIMES.
                   15  SCB-EK-005474        PIC X(10).
                   15  SCB-EV-005474        PIC 9(06) COMP.
               10  SCB-A-005475             PIC 9(09) COMP VALUE 5475.
               10  SCB-B-005476             PIC S9(07)V99 COMP-3 VALUE +0005476.
           05  SCB-ALT-0083 REDEFINES SCB-GRP-0498.
               10  SCB-ALT-FLAG-0083      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0083   PIC X(64).
           05  SCB-GRP-0499.
               10  SCB-C-005477             PIC X(24) VALUE 'FIELD-005477-ALPHA'.
               10  SCB-D-005478             PIC 9(04) VALUE 8346.
               10  SCB-DESC-005478          PIC X(18) VALUE 'DESC-071214'.
               10  SCB-E-005479             OCCURS 5 TIMES.
                   15  SCB-EK-005479        PIC X(10).
                   15  SCB-EV-005479        PIC 9(06) COMP.
               10  SCB-A-005480             PIC 9(09) COMP VALUE 5480.
               10  SCB-B-005481             PIC S9(07)V99 COMP-3 VALUE +0005481.
               10  SCB-C-005482             PIC X(24) VALUE 'FIELD-005482-ALPHA'.
               10  SCB-D-005483             PIC 9(04) VALUE 8381.
               10  SCB-DESC-005483          PIC X(18) VALUE 'DESC-071279'.
               10  SCB-E-005484             OCCURS 2 TIMES.
                   15  SCB-EK-005484        PIC X(10).
                   15  SCB-EV-005484        PIC 9(06) COMP.
               10  SCB-A-005485             PIC 9(09) COMP VALUE 5485.
               10  SCB-B-005486             PIC S9(07)V99 COMP-3 VALUE +0005486.
           05  SCB-GRP-0500.
               10  SCB-C-005487             PIC X(24) VALUE 'FIELD-005487-ALPHA'.
               10  SCB-D-005488             PIC 9(04) VALUE 8416.
               10  SCB-DESC-005488          PIC X(18) VALUE 'DESC-071344'.
               10  SCB-E-005489             OCCURS 3 TIMES.
                   15  SCB-EK-005489        PIC X(10).
                   15  SCB-EV-005489        PIC 9(06) COMP.
               10  SCB-A-005490             PIC 9(09) COMP VALUE 5490.
               10  SCB-B-005491             PIC S9(07)V99 COMP-3 VALUE +0005491.
               10  SCB-C-005492             PIC X(24) VALUE 'FIELD-005492-ALPHA'.
               10  SCB-D-005493             PIC 9(04) VALUE 8451.
               10  SCB-DESC-005493          PIC X(18) VALUE 'DESC-071409'.
               10  SCB-E-005494             OCCURS 4 TIMES.
                   15  SCB-EK-005494        PIC X(10).
                   15  SCB-EV-005494        PIC 9(06) COMP.
               10  SCB-A-005495             PIC 9(09) COMP VALUE 5495.
               10  SCB-B-005496             PIC S9(07)V99 COMP-3 VALUE +0005496.
               10  SCB-C-005497             PIC X(24) VALUE 'FIELD-005497-ALPHA'.
           05  SCB-GRP-0501.
               10  SCB-D-005498             PIC 9(04) VALUE 8486.
               10  SCB-DESC-005498          PIC X(18) VALUE 'DESC-071474'.
               10  SCB-E-005499             OCCURS 5 TIMES.
                   15  SCB-EK-005499        PIC X(10).
                   15  SCB-EV-005499        PIC 9(06) COMP.
               10  SCB-A-005500             PIC 9(09) COMP VALUE 5500.
               10  SCB-B-005501             PIC S9(07)V99 COMP-3 VALUE +0005501.
               10  SCB-C-005502             PIC X(24) VALUE 'FIELD-005502-ALPHA'.
               10  SCB-D-005503             PIC 9(04) VALUE 8521.
               10  SCB-DESC-005503          PIC X(18) VALUE 'DESC-071539'.
               10  SCB-E-005504             OCCURS 2 TIMES.
                   15  SCB-EK-005504        PIC X(10).
                   15  SCB-EV-005504        PIC 9(06) COMP.
               10  SCB-A-005505             PIC 9(09) COMP VALUE 5505.
               10  SCB-B-005506             PIC S9(07)V99 COMP-3 VALUE +0005506.
               10  SCB-C-005507             PIC X(24) VALUE 'FIELD-005507-ALPHA'.
               10  SCB-D-005508             PIC 9(04) VALUE 8556.
               10  SCB-DESC-005508          PIC X(18) VALUE 'DESC-071604'.
               10  SCB-E-005509             OCCURS 3 TIMES.
                   15  SCB-EK-005509        PIC X(10).
                   15  SCB-EV-005509        PIC 9(06) COMP.
           05  SCB-GRP-0502.
               10  SCB-A-005510             PIC 9(09) COMP VALUE 5510.
               10  SCB-B-005511             PIC S9(07)V99 COMP-3 VALUE +0005511.
               10  SCB-C-005512             PIC X(24) VALUE 'FIELD-005512-ALPHA'.
               10  SCB-D-005513             PIC 9(04) VALUE 8591.
               10  SCB-DESC-005513          PIC X(18) VALUE 'DESC-071669'.
               10  SCB-E-005514             OCCURS 4 TIMES.
                   15  SCB-EK-005514        PIC X(10).
                   15  SCB-EV-005514        PIC 9(06) COMP.
               10  SCB-A-005515             PIC 9(09) COMP VALUE 5515.
               10  SCB-B-005516             PIC S9(07)V99 COMP-3 VALUE +0005516.
               10  SCB-C-005517             PIC X(24) VALUE 'FIELD-005517-ALPHA'.
               10  SCB-D-005518             PIC 9(04) VALUE 8626.
               10  SCB-DESC-005518          PIC X(18) VALUE 'DESC-071734'.
               10  SCB-E-005519             OCCURS 5 TIMES.
                   15  SCB-EK-005519        PIC X(10).
                   15  SCB-EV-005519        PIC 9(06) COMP.
               10  SCB-A-005520             PIC 9(09) COMP VALUE 5520.
               10  SCB-B-005521             PIC S9(07)V99 COMP-3 VALUE +0005521.
               10  SCB-C-005522             PIC X(24) VALUE 'FIELD-005522-ALPHA'.
           05  SCB-GRP-0503.
               10  SCB-D-005523             PIC 9(04) VALUE 8661.
               10  SCB-DESC-005523          PIC X(18) VALUE 'DESC-071799'.
               10  SCB-E-005524             OCCURS 2 TIMES.
                   15  SCB-EK-005524        PIC X(10).
                   15  SCB-EV-005524        PIC 9(06) COMP.
               10  SCB-A-005525             PIC 9(09) COMP VALUE 5525.
               10  SCB-B-005526             PIC S9(07)V99 COMP-3 VALUE +0005526.
               10  SCB-C-005527             PIC X(24) VALUE 'FIELD-005527-ALPHA'.
               10  SCB-D-005528             PIC 9(04) VALUE 8696.
               10  SCB-DESC-005528          PIC X(18) VALUE 'DESC-071864'.
               10  SCB-E-005529             OCCURS 3 TIMES.
                   15  SCB-EK-005529        PIC X(10).
                   15  SCB-EV-005529        PIC 9(06) COMP.
               10  SCB-A-005530             PIC 9(09) COMP VALUE 5530.
               10  SCB-B-005531             PIC S9(07)V99 COMP-3 VALUE +0005531.
               10  SCB-C-005532             PIC X(24) VALUE 'FIELD-005532-ALPHA'.
               10  SCB-D-005533             PIC 9(04) VALUE 8731.
               10  SCB-DESC-005533          PIC X(18) VALUE 'DESC-071929'.
               10  SCB-E-005534             OCCURS 4 TIMES.
                   15  SCB-EK-005534        PIC X(10).
                   15  SCB-EV-005534        PIC 9(06) COMP.
               10  SCB-A-005535             PIC 9(09) COMP VALUE 5535.
               10  SCB-B-005536             PIC S9(07)V99 COMP-3 VALUE +0005536.
           05  SCB-GRP-0504.
               10  SCB-C-005537             PIC X(24) VALUE 'FIELD-005537-ALPHA'.
               10  SCB-D-005538             PIC 9(04) VALUE 8766.
               10  SCB-DESC-005538          PIC X(18) VALUE 'DESC-071994'.
               10  SCB-E-005539             OCCURS 5 TIMES.
                   15  SCB-EK-005539        PIC X(10).
                   15  SCB-EV-005539        PIC 9(06) COMP.
               10  SCB-A-005540             PIC 9(09) COMP VALUE 5540.
               10  SCB-B-005541             PIC S9(07)V99 COMP-3 VALUE +0005541.
               10  SCB-C-005542             PIC X(24) VALUE 'FIELD-005542-ALPHA'.
               10  SCB-D-005543             PIC 9(04) VALUE 8801.
               10  SCB-DESC-005543          PIC X(18) VALUE 'DESC-072059'.
               10  SCB-E-005544             OCCURS 2 TIMES.
                   15  SCB-EK-005544        PIC X(10).
                   15  SCB-EV-005544        PIC 9(06) COMP.
           05  SCB-ALT-0084 REDEFINES SCB-GRP-0504.
               10  SCB-ALT-FLAG-0084      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0084   PIC X(64).
           05  SCB-GRP-0505.
               10  SCB-A-005545             PIC 9(09) COMP VALUE 5545.
               10  SCB-B-005546             PIC S9(07)V99 COMP-3 VALUE +0005546.
               10  SCB-C-005547             PIC X(24) VALUE 'FIELD-005547-ALPHA'.
               10  SCB-D-005548             PIC 9(04) VALUE 8836.
               10  SCB-DESC-005548          PIC X(18) VALUE 'DESC-072124'.
               10  SCB-E-005549             OCCURS 3 TIMES.
                   15  SCB-EK-005549        PIC X(10).
                   15  SCB-EV-005549        PIC 9(06) COMP.
               10  SCB-A-005550             PIC 9(09) COMP VALUE 5550.
               10  SCB-B-005551             PIC S9(07)V99 COMP-3 VALUE +0005551.
               10  SCB-C-005552             PIC X(24) VALUE 'FIELD-005552-ALPHA'.
               10  SCB-D-005553             PIC 9(04) VALUE 8871.
               10  SCB-DESC-005553          PIC X(18) VALUE 'DESC-072189'.
           05  SCB-GRP-0506.
               10  SCB-E-005554             OCCURS 4 TIMES.
                   15  SCB-EK-005554        PIC X(10).
                   15  SCB-EV-005554        PIC 9(06) COMP.
               10  SCB-A-005555             PIC 9(09) COMP VALUE 5555.
               10  SCB-B-005556             PIC S9(07)V99 COMP-3 VALUE +0005556.
               10  SCB-C-005557             PIC X(24) VALUE 'FIELD-005557-ALPHA'.
               10  SCB-D-005558             PIC 9(04) VALUE 8906.
               10  SCB-DESC-005558          PIC X(18) VALUE 'DESC-072254'.
               10  SCB-E-005559             OCCURS 5 TIMES.
                   15  SCB-EK-005559        PIC X(10).
                   15  SCB-EV-005559        PIC 9(06) COMP.
               10  SCB-A-005560             PIC 9(09) COMP VALUE 5560.
               10  SCB-B-005561             PIC S9(07)V99 COMP-3 VALUE +0005561.
               10  SCB-C-005562             PIC X(24) VALUE 'FIELD-005562-ALPHA'.
               10  SCB-D-005563             PIC 9(04) VALUE 8941.
               10  SCB-DESC-005563          PIC X(18) VALUE 'DESC-072319'.
           05  SCB-GRP-0507.
               10  SCB-E-005564             OCCURS 2 TIMES.
                   15  SCB-EK-005564        PIC X(10).
                   15  SCB-EV-005564        PIC 9(06) COMP.
               10  SCB-A-005565             PIC 9(09) COMP VALUE 5565.
               10  SCB-B-005566             PIC S9(07)V99 COMP-3 VALUE +0005566.
               10  SCB-C-005567             PIC X(24) VALUE 'FIELD-005567-ALPHA'.
               10  SCB-D-005568             PIC 9(04) VALUE 8976.
               10  SCB-DESC-005568          PIC X(18) VALUE 'DESC-072384'.
               10  SCB-E-005569             OCCURS 3 TIMES.
                   15  SCB-EK-005569        PIC X(10).
                   15  SCB-EV-005569        PIC 9(06) COMP.
               10  SCB-A-005570             PIC 9(09) COMP VALUE 5570.
               10  SCB-B-005571             PIC S9(07)V99 COMP-3 VALUE +0005571.
               10  SCB-C-005572             PIC X(24) VALUE 'FIELD-005572-ALPHA'.
               10  SCB-D-005573             PIC 9(04) VALUE 9011.
               10  SCB-DESC-005573          PIC X(18) VALUE 'DESC-072449'.
               10  SCB-E-005574             OCCURS 4 TIMES.
                   15  SCB-EK-005574        PIC X(10).
                   15  SCB-EV-005574        PIC 9(06) COMP.
           05  SCB-GRP-0508.
               10  SCB-A-005575             PIC 9(09) COMP VALUE 5575.
               10  SCB-B-005576             PIC S9(07)V99 COMP-3 VALUE +0005576.
               10  SCB-C-005577             PIC X(24) VALUE 'FIELD-005577-ALPHA'.
               10  SCB-D-005578             PIC 9(04) VALUE 9046.
               10  SCB-DESC-005578          PIC X(18) VALUE 'DESC-072514'.
               10  SCB-E-005579             OCCURS 5 TIMES.
                   15  SCB-EK-005579        PIC X(10).
                   15  SCB-EV-005579        PIC 9(06) COMP.
               10  SCB-A-005580             PIC 9(09) COMP VALUE 5580.
               10  SCB-B-005581             PIC S9(07)V99 COMP-3 VALUE +0005581.
               10  SCB-C-005582             PIC X(24) VALUE 'FIELD-005582-ALPHA'.
               10  SCB-D-005583             PIC 9(04) VALUE 9081.
               10  SCB-DESC-005583          PIC X(18) VALUE 'DESC-072579'.
               10  SCB-E-005584             OCCURS 2 TIMES.
                   15  SCB-EK-005584        PIC X(10).
                   15  SCB-EV-005584        PIC 9(06) COMP.
               10  SCB-A-005585             PIC 9(09) COMP VALUE 5585.
               10  SCB-B-005586             PIC S9(07)V99 COMP-3 VALUE +0005586.
           05  SCB-GRP-0509.
               10  SCB-C-005587             PIC X(24) VALUE 'FIELD-005587-ALPHA'.
               10  SCB-D-005588             PIC 9(04) VALUE 9116.
               10  SCB-DESC-005588          PIC X(18) VALUE 'DESC-072644'.
               10  SCB-E-005589             OCCURS 3 TIMES.
                   15  SCB-EK-005589        PIC X(10).
                   15  SCB-EV-005589        PIC 9(06) COMP.
               10  SCB-A-005590             PIC 9(09) COMP VALUE 5590.
               10  SCB-B-005591             PIC S9(07)V99 COMP-3 VALUE +0005591.
               10  SCB-C-005592             PIC X(24) VALUE 'FIELD-005592-ALPHA'.
               10  SCB-D-005593             PIC 9(04) VALUE 9151.
               10  SCB-DESC-005593          PIC X(18) VALUE 'DESC-072709'.
               10  SCB-E-005594             OCCURS 4 TIMES.
                   15  SCB-EK-005594        PIC X(10).
                   15  SCB-EV-005594        PIC 9(06) COMP.
               10  SCB-A-005595             PIC 9(09) COMP VALUE 5595.
               10  SCB-B-005596             PIC S9(07)V99 COMP-3 VALUE +0005596.
               10  SCB-C-005597             PIC X(24) VALUE 'FIELD-005597-ALPHA'.
               10  SCB-D-005598             PIC 9(04) VALUE 9186.
               10  SCB-DESC-005598          PIC X(18) VALUE 'DESC-072774'.
               10  SCB-E-005599             OCCURS 5 TIMES.
                   15  SCB-EK-005599        PIC X(10).
                   15  SCB-EV-005599        PIC 9(06) COMP.
           05  SCB-GRP-0510.
               10  SCB-A-005600             PIC 9(09) COMP VALUE 5600.
               10  SCB-B-005601             PIC S9(07)V99 COMP-3 VALUE +0005601.
               10  SCB-C-005602             PIC X(24) VALUE 'FIELD-005602-ALPHA'.
               10  SCB-D-005603             PIC 9(04) VALUE 9221.
               10  SCB-DESC-005603          PIC X(18) VALUE 'DESC-072839'.
               10  SCB-E-005604             OCCURS 2 TIMES.
                   15  SCB-EK-005604        PIC X(10).
                   15  SCB-EV-005604        PIC 9(06) COMP.
               10  SCB-A-005605             PIC 9(09) COMP VALUE 5605.
               10  SCB-B-005606             PIC S9(07)V99 COMP-3 VALUE +0005606.
               10  SCB-C-005607             PIC X(24) VALUE 'FIELD-005607-ALPHA'.
               10  SCB-D-005608             PIC 9(04) VALUE 9256.
               10  SCB-DESC-005608          PIC X(18) VALUE 'DESC-072904'.
               10  SCB-E-005609             OCCURS 3 TIMES.
                   15  SCB-EK-005609        PIC X(10).
                   15  SCB-EV-005609        PIC 9(06) COMP.
               10  SCB-A-005610             PIC 9(09) COMP VALUE 5610.
               10  SCB-B-005611             PIC S9(07)V99 COMP-3 VALUE +0005611.
               10  SCB-C-005612             PIC X(24) VALUE 'FIELD-005612-ALPHA'.
               10  SCB-D-005613             PIC 9(04) VALUE 9291.
               10  SCB-DESC-005613          PIC X(18) VALUE 'DESC-072969'.
           05  SCB-ALT-0085 REDEFINES SCB-GRP-0510.
               10  SCB-ALT-FLAG-0085      PIC X VALUE 'N'.
               10  SCB-ALT-PAYLOAD-0085   PIC X(64).
           05  SCB-GRP-0511.
               10  SCB-E-005614             OCCURS 4 TIMES.
                   15  SCB-EK-005614        PIC X(10).
                   15  SCB-EV-005614        PIC 9(06) COMP.
               10  SCB-A-005615             PIC 9(09) COMP VALUE 5615.
               10  SCB-B-005616             PIC S9(07)V99 COMP-3 VALUE +0005616.
               10  SCB-C-005617             PIC X(24) VALUE 'FIELD-005617-ALPHA'.
               10  SCB-D-005618             PIC 9(04) VALUE 9326.
               10  SCB-DESC-005618          PIC X(18) VALUE 'DESC-073034'.
               10  SCB-E-005619             OCCURS 5 TIMES.
                   15  SCB-EK-005619        PIC X(10).
                   15  SCB-EV-005619        PIC 9(06) COMP.
               10  SCB-A-005620             PIC 9(09) COMP VALUE 5620.
               10  SCB-B-005621             PIC S9(07)V99 COMP-3 VALUE +0005621.
           05  SCB-GRP-0512.
               10  SCB-C-005622             PIC X(24) VALUE 'FIELD-005622-ALPHA'.
               10  SCB-D-005623             PIC 9(04) VALUE 9361.
               10  SCB-DESC-005623          PIC X(18) VALUE 'DESC-073099'.
               10  SCB-E-005624             OCCURS 2 TIMES.
                   15  SCB-EK-005624        PIC X(10).
                   15  SCB-EV-005624        PIC 9(06) COMP.
               10  SCB-A-005625             PIC 9(09) COMP VALUE 5625.
               10  SCB-B-005626             PIC S9(07)V99 COMP-3 VALUE +0005626.
               10  SCB-C-005627             PIC X(24) VALUE 'FIELD-005627-ALPHA'.
               10  SCB-D-005628             PIC 9(04) VALUE 9396.
               10  SCB-DESC-005628          PIC X(18) VALUE 'DESC-073164'.
               10  SCB-E-005629             OCCURS 3 TIMES.
                   15  SCB-EK-005629        PIC X(10).
                   15  SCB-EV-005629        PIC 9(06) COMP.
               10  SCB-A-005630             PIC 9(09) COMP VALUE 5630.
           05  SCB-LOOKUP-TABLES.
               10  SCB-TBL-01             OCCURS 11 TIMES INDEXED BY SCB-IX-01.
                   15  SCB-TBL-KEY-01     PIC X(12).
                   15  SCB-TBL-VAL-01     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-01     PIC X(08).
               10  SCB-TBL-02             OCCURS 12 TIMES INDEXED BY SCB-IX-02.
                   15  SCB-TBL-KEY-02     PIC X(12).
                   15  SCB-TBL-VAL-02     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-02     PIC X(08).
               10  SCB-TBL-03             OCCURS 13 TIMES INDEXED BY SCB-IX-03.
                   15  SCB-TBL-KEY-03     PIC X(12).
                   15  SCB-TBL-VAL-03     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-03     PIC X(08).
               10  SCB-TBL-04             OCCURS 14 TIMES INDEXED BY SCB-IX-04.
                   15  SCB-TBL-KEY-04     PIC X(12).
                   15  SCB-TBL-VAL-04     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-04     PIC X(08).
               10  SCB-TBL-05             OCCURS 15 TIMES INDEXED BY SCB-IX-05.
                   15  SCB-TBL-KEY-05     PIC X(12).
                   15  SCB-TBL-VAL-05     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-05     PIC X(08).
               10  SCB-TBL-06             OCCURS 16 TIMES INDEXED BY SCB-IX-06.
                   15  SCB-TBL-KEY-06     PIC X(12).
                   15  SCB-TBL-VAL-06     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-06     PIC X(08).
               10  SCB-TBL-07             OCCURS 17 TIMES INDEXED BY SCB-IX-07.
                   15  SCB-TBL-KEY-07     PIC X(12).
                   15  SCB-TBL-VAL-07     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-07     PIC X(08).
               10  SCB-TBL-08             OCCURS 18 TIMES INDEXED BY SCB-IX-08.
                   15  SCB-TBL-KEY-08     PIC X(12).
                   15  SCB-TBL-VAL-08     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-08     PIC X(08).
               10  SCB-TBL-09             OCCURS 19 TIMES INDEXED BY SCB-IX-09.
                   15  SCB-TBL-KEY-09     PIC X(12).
                   15  SCB-TBL-VAL-09     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-09     PIC X(08).
               10  SCB-TBL-10             OCCURS 20 TIMES INDEXED BY SCB-IX-10.
                   15  SCB-TBL-KEY-10     PIC X(12).
                   15  SCB-TBL-VAL-10     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-10     PIC X(08).
               10  SCB-TBL-11             OCCURS 10 TIMES INDEXED BY SCB-IX-11.
                   15  SCB-TBL-KEY-11     PIC X(12).
                   15  SCB-TBL-VAL-11     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-11     PIC X(08).
               10  SCB-TBL-12             OCCURS 11 TIMES INDEXED BY SCB-IX-12.
                   15  SCB-TBL-KEY-12     PIC X(12).
                   15  SCB-TBL-VAL-12     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-12     PIC X(08).
               10  SCB-TBL-13             OCCURS 12 TIMES INDEXED BY SCB-IX-13.
                   15  SCB-TBL-KEY-13     PIC X(12).
                   15  SCB-TBL-VAL-13     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-13     PIC X(08).
               10  SCB-TBL-14             OCCURS 13 TIMES INDEXED BY SCB-IX-14.
                   15  SCB-TBL-KEY-14     PIC X(12).
                   15  SCB-TBL-VAL-14     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-14     PIC X(08).
               10  SCB-TBL-15             OCCURS 14 TIMES INDEXED BY SCB-IX-15.
                   15  SCB-TBL-KEY-15     PIC X(12).
                   15  SCB-TBL-VAL-15     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-15     PIC X(08).
               10  SCB-TBL-16             OCCURS 15 TIMES INDEXED BY SCB-IX-16.
                   15  SCB-TBL-KEY-16     PIC X(12).
                   15  SCB-TBL-VAL-16     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-16     PIC X(08).
               10  SCB-TBL-17             OCCURS 16 TIMES INDEXED BY SCB-IX-17.
                   15  SCB-TBL-KEY-17     PIC X(12).
                   15  SCB-TBL-VAL-17     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-17     PIC X(08).
               10  SCB-TBL-18             OCCURS 17 TIMES INDEXED BY SCB-IX-18.
                   15  SCB-TBL-KEY-18     PIC X(12).
                   15  SCB-TBL-VAL-18     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-18     PIC X(08).
               10  SCB-TBL-19             OCCURS 18 TIMES INDEXED BY SCB-IX-19.
                   15  SCB-TBL-KEY-19     PIC X(12).
                   15  SCB-TBL-VAL-19     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-19     PIC X(08).
               10  SCB-TBL-20             OCCURS 19 TIMES INDEXED BY SCB-IX-20.
                   15  SCB-TBL-KEY-20     PIC X(12).
                   15  SCB-TBL-VAL-20     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-20     PIC X(08).
               10  SCB-TBL-21             OCCURS 20 TIMES INDEXED BY SCB-IX-21.
                   15  SCB-TBL-KEY-21     PIC X(12).
                   15  SCB-TBL-VAL-21     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-21     PIC X(08).
               10  SCB-TBL-22             OCCURS 10 TIMES INDEXED BY SCB-IX-22.
                   15  SCB-TBL-KEY-22     PIC X(12).
                   15  SCB-TBL-VAL-22     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-22     PIC X(08).
               10  SCB-TBL-23             OCCURS 11 TIMES INDEXED BY SCB-IX-23.
                   15  SCB-TBL-KEY-23     PIC X(12).
                   15  SCB-TBL-VAL-23     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-23     PIC X(08).
               10  SCB-TBL-24             OCCURS 12 TIMES INDEXED BY SCB-IX-24.
                   15  SCB-TBL-KEY-24     PIC X(12).
                   15  SCB-TBL-VAL-24     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-24     PIC X(08).
               10  SCB-TBL-25             OCCURS 13 TIMES INDEXED BY SCB-IX-25.
                   15  SCB-TBL-KEY-25     PIC X(12).
                   15  SCB-TBL-VAL-25     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-25     PIC X(08).
               10  SCB-TBL-26             OCCURS 14 TIMES INDEXED BY SCB-IX-26.
                   15  SCB-TBL-KEY-26     PIC X(12).
                   15  SCB-TBL-VAL-26     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-26     PIC X(08).
               10  SCB-TBL-27             OCCURS 15 TIMES INDEXED BY SCB-IX-27.
                   15  SCB-TBL-KEY-27     PIC X(12).
                   15  SCB-TBL-VAL-27     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-27     PIC X(08).
               10  SCB-TBL-28             OCCURS 16 TIMES INDEXED BY SCB-IX-28.
                   15  SCB-TBL-KEY-28     PIC X(12).
                   15  SCB-TBL-VAL-28     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-28     PIC X(08).
               10  SCB-TBL-29             OCCURS 17 TIMES INDEXED BY SCB-IX-29.
                   15  SCB-TBL-KEY-29     PIC X(12).
                   15  SCB-TBL-VAL-29     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-29     PIC X(08).
               10  SCB-TBL-30             OCCURS 18 TIMES INDEXED BY SCB-IX-30.
                   15  SCB-TBL-KEY-30     PIC X(12).
                   15  SCB-TBL-VAL-30     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-30     PIC X(08).
               10  SCB-TBL-31             OCCURS 19 TIMES INDEXED BY SCB-IX-31.
                   15  SCB-TBL-KEY-31     PIC X(12).
                   15  SCB-TBL-VAL-31     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-31     PIC X(08).
               10  SCB-TBL-32             OCCURS 20 TIMES INDEXED BY SCB-IX-32.
                   15  SCB-TBL-KEY-32     PIC X(12).
                   15  SCB-TBL-VAL-32     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-32     PIC X(08).
               10  SCB-TBL-33             OCCURS 10 TIMES INDEXED BY SCB-IX-33.
                   15  SCB-TBL-KEY-33     PIC X(12).
                   15  SCB-TBL-VAL-33     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-33     PIC X(08).
               10  SCB-TBL-34             OCCURS 11 TIMES INDEXED BY SCB-IX-34.
                   15  SCB-TBL-KEY-34     PIC X(12).
                   15  SCB-TBL-VAL-34     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-34     PIC X(08).
               10  SCB-TBL-35             OCCURS 12 TIMES INDEXED BY SCB-IX-35.
                   15  SCB-TBL-KEY-35     PIC X(12).
                   15  SCB-TBL-VAL-35     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-35     PIC X(08).
               10  SCB-TBL-36             OCCURS 13 TIMES INDEXED BY SCB-IX-36.
                   15  SCB-TBL-KEY-36     PIC X(12).
                   15  SCB-TBL-VAL-36     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-36     PIC X(08).
               10  SCB-TBL-37             OCCURS 14 TIMES INDEXED BY SCB-IX-37.
                   15  SCB-TBL-KEY-37     PIC X(12).
                   15  SCB-TBL-VAL-37     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-37     PIC X(08).
               10  SCB-TBL-38             OCCURS 15 TIMES INDEXED BY SCB-IX-38.
                   15  SCB-TBL-KEY-38     PIC X(12).
                   15  SCB-TBL-VAL-38     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-38     PIC X(08).
               10  SCB-TBL-39             OCCURS 16 TIMES INDEXED BY SCB-IX-39.
                   15  SCB-TBL-KEY-39     PIC X(12).
                   15  SCB-TBL-VAL-39     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-39     PIC X(08).
               10  SCB-TBL-40             OCCURS 17 TIMES INDEXED BY SCB-IX-40.
                   15  SCB-TBL-KEY-40     PIC X(12).
                   15  SCB-TBL-VAL-40     PIC 9(09) COMP.
                   15  SCB-TBL-TAG-40     PIC X(08).
       * PAD-COPYBOOK-LINE-09947 ***********************************************
       * PAD-COPYBOOK-LINE-09948 ***********************************************
       * PAD-COPYBOOK-LINE-09949 ***********************************************
       * PAD-COPYBOOK-LINE-09950 ***********************************************
       * PAD-COPYBOOK-LINE-09951 ***********************************************
       * PAD-COPYBOOK-LINE-09952 ***********************************************
       * PAD-COPYBOOK-LINE-09953 ***********************************************
       * PAD-COPYBOOK-LINE-09954 ***********************************************
       * PAD-COPYBOOK-LINE-09955 ***********************************************
       * PAD-COPYBOOK-LINE-09956 ***********************************************
       * PAD-COPYBOOK-LINE-09957 ***********************************************
       * PAD-COPYBOOK-LINE-09958 ***********************************************
       * PAD-COPYBOOK-LINE-09959 ***********************************************
       * PAD-COPYBOOK-LINE-09960 ***********************************************
       * PAD-COPYBOOK-LINE-09961 ***********************************************
       * PAD-COPYBOOK-LINE-09962 ***********************************************
       * PAD-COPYBOOK-LINE-09963 ***********************************************
       * PAD-COPYBOOK-LINE-09964 ***********************************************
       * PAD-COPYBOOK-LINE-09965 ***********************************************
       * PAD-COPYBOOK-LINE-09966 ***********************************************
       * PAD-COPYBOOK-LINE-09967 ***********************************************
       * PAD-COPYBOOK-LINE-09968 ***********************************************
       * PAD-COPYBOOK-LINE-09969 ***********************************************
       * PAD-COPYBOOK-LINE-09970 ***********************************************
       * PAD-COPYBOOK-LINE-09971 ***********************************************
       * PAD-COPYBOOK-LINE-09972 ***********************************************
       * PAD-COPYBOOK-LINE-09973 ***********************************************
       * PAD-COPYBOOK-LINE-09974 ***********************************************
       * PAD-COPYBOOK-LINE-09975 ***********************************************
       * PAD-COPYBOOK-LINE-09976 ***********************************************
       * PAD-COPYBOOK-LINE-09977 ***********************************************
       * PAD-COPYBOOK-LINE-09978 ***********************************************
       * PAD-COPYBOOK-LINE-09979 ***********************************************
       * PAD-COPYBOOK-LINE-09980 ***********************************************
       * PAD-COPYBOOK-LINE-09981 ***********************************************
       * PAD-COPYBOOK-LINE-09982 ***********************************************
       * PAD-COPYBOOK-LINE-09983 ***********************************************
       * PAD-COPYBOOK-LINE-09984 ***********************************************
       * PAD-COPYBOOK-LINE-09985 ***********************************************
       * PAD-COPYBOOK-LINE-09986 ***********************************************
       * PAD-COPYBOOK-LINE-09987 ***********************************************
       * PAD-COPYBOOK-LINE-09988 ***********************************************
       * PAD-COPYBOOK-LINE-09989 ***********************************************
       * PAD-COPYBOOK-LINE-09990 ***********************************************
       * PAD-COPYBOOK-LINE-09991 ***********************************************
       * PAD-COPYBOOK-LINE-09992 ***********************************************
       * PAD-COPYBOOK-LINE-09993 ***********************************************
       * PAD-COPYBOOK-LINE-09994 ***********************************************
       * PAD-COPYBOOK-LINE-09995 ***********************************************
       * PAD-COPYBOOK-LINE-09996 ***********************************************
       * PAD-COPYBOOK-LINE-09997 ***********************************************
       * PAD-COPYBOOK-LINE-09998 ***********************************************
       * PAD-COPYBOOK-LINE-09999 ***********************************************
       * PAD-COPYBOOK-LINE-10000 ***********************************************
