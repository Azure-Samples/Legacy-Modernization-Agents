******* Copyright (c) 2021 Broadcom.
******* The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
*******
******* This program and the accompanying materials are made
******* available under the terms of the Eclipse Public License 2.0
******* which is available at https://www.eclipse.org/legal/epl-2.0/
*******
******* SPDX-License-Identifier: EPL-2.0
*******
******* Contributors:
******* Broadcom, Inc. - initial API and implementation
        01 SQLDA.
            05 SQLDAID PIC X(8).
            05 SQLDABC PIC S9(9) BINARY.
            05 SQLN    PIC S9(4) BINARY.
            05 SQLD    PIC S9(4) BINARY.
            05 SQLVAR  OCCURS 0 TO 750 TIMES DEPENDING ON SQLN.
                10 SQLVAR1.
                        15 SQLTYPE PIC S9(4) BINARY.
                        15 SQLLEN  PIC S9(4) BINARY.
                        15 FILLER  REDEFINES SQLLEN.
                        20 SQLPRECISION   PIC X.
                        20 SQLSCALE       PIC X.
                        15 SQLDATA POINTER.
                        15 SQLIND  POINTER.
                        15 SQLNAME.
                        49 SQLNAMEL PIC S9(4) BINARY.
                        49 SQLNAMEC PIC X(30).
               10 SQLVAR2 REDEFINES SQLVAR1.
                15 SQLVAR2-RESERVED-1 PIC S9(9) BINARY.
                15 SQLLONGLEN  REDEFINES SQLVAR2-RESERVED-1
                                PIC S9(9) BINARY.
                15 SQLVAR2-RESERVED-2 PIC S9(9) BINARY.
                15 SQLDATALEN         POINTER.
                15 SQLDATATYPE-NAME.
                20 SQLDATATYPE-NAMEL PIC S9(4) BINARY.
                20 SQLDATATYPE-NAMEC PIC X(30).
