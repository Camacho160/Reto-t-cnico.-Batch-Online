      ******************************************************************
      * Author: Carlos Fernando Camacho Agón
      * Date:   2026/04/18
      * Purpose: Programa encargado de leer un archivo .TXT para
      *          realizar  el respectivo proceso de datos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DatosBatch.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *----Select para archivo secuencial.
           SELECT DATOS-TXT
               ASSIGN TO "C:\Users\USUARIO\Pr_Banco_Bogota\DATOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-TXT.

      *----Select para archivo Indexado
           SELECT CUENTAS
               ASSIGN TO "C:\Users\USUARIO\Pr_Banco_Bogota\CUENTAS"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS LV0-CTA
               FILE STATUS  IS ST-CTA.

      *----Select para archivo de salida
           SELECT ARCHIVO-SALE
               ASSIGN TO "C:\Users\USUARIO\Pr_Banco_Bogota\SALE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

      *----Select para archivo Log
           SELECT ARCHIVO-LOG
               ASSIGN TO "C:\Users\USUARIO\Pr_Banco_Bogota\LOG.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS ST-LOG.

       DATA DIVISION.
       FILE SECTION.
       FD  DATOS-TXT.
       01  REG-LINEA PIC X(50).

       FD  CUENTAS.
       01  REG-CUENTAS.
           02 LV0-CTA       PIC X(06).
           02 TP-OPERACION  PIC X.
           02 VLR-SALDO     PIC 9(12).
           02 CAMPOS-DISPO  PIC X(100).
      *----------------------------------------------------------------*
      *    LV0-CTA        : LLAVE UNICA. Número de cuenta              *
      *    VLR-SALDO     : Tipo de operación realizada                 *
      *                     D -> Deposito                              *
      *                     W -> Retiro                                *
      *    VLR-OPERACION : Valor de la operación.                      *
      *    CAMPOS-DISPO  : Campos disponibles                          *
      *----------------------------------------------------------------*

       FD  ARCHIVO-SALE.
           01 REG-SALE      PIC X(60).

       FD  ARCHIVO-LOG.
       01  REG-LOG.
           02 FECHA-LOG  PIC X(11).
           02 HORA-LOG   PIC X(12).
           02 MENSAJE    PIC X(200).


       WORKING-STORAGE SECTION.
       01  WK-VARIABLES-TXT.
           02 WK-CTA PIC X(06).
           02 FILLER PIC X     VALUE "|".
           02 WK-DES PIC X(05) VALUE "SALDO".
           02 FILLER PIC X     VALUE "|".
           02 WK-VLR PIC 9(12).
           02 FILLER PIC X     VALUE "|".

       01  WK-FILE-STATUS.
           02 ST-CTA       PIC XX.
           02 ST-TXT       PIC XX.
           02 ST-LOG       PIC XX.



       01  WK-VARIABLES-TRABAJO.
           02 SW           PIC 9.
           02 SW-INICIO    PIC 9 VALUE ZERO.
           02 WK-REG-TEM.
               03 WK-NRO-CTA   PIC X(09).
               03 WK-OPERACION PIC X.
               03 WK-MONTO     PIC X(12).
           02 WK-MONTO-TEM PIC 9(12).
           02 WK-CTA-TEM   PIC X(06).
           02 WK-MENSAJE   PIC X(200).
           02 WK-MSN-LOG   PIC X(200).
           02 WK-HORA-SIS  PIC 9(08).
           02 WK-FECHA-SIS PIC 9(08).



       PROCEDURE DIVISION.

       DECLARATIVES.

       ERROR-DATOS SECTION.
       USE AFTER ERROR PROCEDURE ON DATOS-TXT.

       ERROR-DATOS-LOGIC.
       IF ST-TXT = "35"
           MOVE 1  TO SW-INICIO
           MOVE 35 TO RETURN-CODE
           MOVE "El archivo DATOS.TXT no existe"
           TO WK-MENSAJE
       END-IF.

       ERROR-CUENTAS SECTION.
       USE AFTER ERROR PROCEDURE ON CUENTAS.

       ERROR-CUENTAS-LOGIC.
       IF  ST-CTA = "35"
           OPEN OUTPUT CUENTAS
           CLOSE CUENTAS
           OPEN I-O CUENTAS
       END-IF.

       ERROR-LOG SECTION.
       USE AFTER ERROR PROCEDURE ON ARCHIVO-LOG.

       ERROR-LOG-LOGIC.
       IF  ST-LOG = "35"
           OPEN OUTPUT ARCHIVO-LOG
       END-IF.

       END DECLARATIVES.

       MAIN-PROCEDURE.
       0000-INICIA-PROGRAMA.
           PERFORM 0005-ABRIR-ARCHIVOS
           MOVE "Inicia proceso en el programa DatosBatch" TO WK-MSN-LOG
           PERFORM 090-ESCRIBIR-LOG

           IF SW-INICIO EQUAL ZERO
               MOVE 0 TO SW RETURN-CODE
               OPEN OUTPUT ARCHIVO-SALE
               PERFORM 0010-LEER-TXT UNTIL SW EQUAL 1
               CLOSE ARCHIVO-SALE
           ELSE
               DISPLAY WK-MENSAJE
               MOVE "No se realiza proceso Batch por error 35 en .TXT"
               TO WK-MSN-LOG PERFORM 090-ESCRIBIR-LOG
           END-IF

           INITIALIZE WK-MSN-LOG
           MOVE "Finaliza proceso en el programa DatosBatch"
           TO WK-MSN-LOG
           PERFORM 090-ESCRIBIR-LOG

           PERFORM 0100-CERRAR-ARCHIVOS

           IF RETURN-CODE EQUAL ZEROS
               DISPLAY "PROCESO FINALIZADO: COD: 00"
           ELSE
               DISPLAY "PROCESO FINALIZADO CON ERRORES: COD: 35"
           END-IF.

           STOP RUN.

       0005-ABRIR-ARCHIVOS.
           OPEN I-O    CUENTAS
           OPEN INPUT  DATOS-TXT
           OPEN EXTEND ARCHIVO-LOG

           INITIALIZE  WK-MSN-LOG.

       0010-LEER-TXT.
           READ DATOS-TXT WITH NO LOCK AT END MOVE 1 TO SW
           NOT AT END
              PERFORM 0015-LIMPIAR-REG
           END-READ.

       0015-LIMPIAR-REG.
           UNSTRING REG-LINEA DELIMITED BY "|"
               INTO WK-NRO-CTA WK-OPERACION WK-MONTO
           END-UNSTRING
           MOVE WK-MONTO TO WK-MONTO-TEM

           IF WK-OPERACION EQUAL "D" OR WK-OPERACION EQUAL "W"
               PERFORM 0020-LEER-CUENTAS
           END-IF.

       0020-LEER-CUENTAS.
           MOVE WK-NRO-CTA TO LV0-CTA

           READ CUENTAS WITH NO LOCK KEY IS LV0-CTA INVALID KEY
               PERFORM 0025-GRABAR-DATOS
           NOT INVALID KEY
               PERFORM 0030-REGRABAR-DATOS
           END-READ
           PERFORM 0035-ARCHIVO-SALIDA.


       0025-GRABAR-DATOS.
           INITIALIZE REG-CUENTAS
           MOVE WK-NRO-CTA   TO LV0-CTA
           MOVE WK-OPERACION TO TP-OPERACION
           MOVE WK-MONTO-TEM TO VLR-SALDO

           WRITE REG-CUENTAS INVALID KEY
               INITIALIZE WK-MSN-LOG
               MOVE "ERROR AL GRABAR LOS DATOS DE LA CUENTA: "
               TO WK-MSN-LOG
               MOVE LV0-CTA TO WK-MSN-LOG(40:)
               PERFORM 090-ESCRIBIR-LOG
           END-WRITE.

       0030-REGRABAR-DATOS.
           MOVE WK-OPERACION  TO TP-OPERACION

           IF WK-OPERACION EQUAL "D"
               COMPUTE VLR-SALDO = VLR-SALDO + WK-MONTO-TEM
           ELSE
               COMPUTE VLR-SALDO = VLR-SALDO - WK-MONTO-TEM
           END-IF

           REWRITE REG-CUENTAS INVALID KEY
               INITIALIZE WK-MSN-LOG
               MOVE "ERROR AL REGRABAR LOS DATOS DE LA CUENTA: "
               TO WK-MSN-LOG
               MOVE LV0-CTA TO WK-MSN-LOG(40:)
               PERFORM 090-ESCRIBIR-LOG
           END-REWRITE.

       0035-ARCHIVO-SALIDA.
           INITIALIZE WK-CTA WK-VLR
           MOVE LV0-CTA       TO WK-CTA
           MOVE VLR-SALDO     TO WK-VLR
           WRITE REG-SALE FROM WK-VARIABLES-TXT END-WRITE.


       090-ESCRIBIR-LOG.
           ACCEPT WK-FECHA-SIS FROM DATE
           ACCEPT WK-HORA-SIS  FROM TIME

           MOVE "20"              TO FECHA-LOG(1:2)
           MOVE WK-FECHA-SIS(3:2) TO FECHA-LOG(3:2)
           MOVE "/"               TO FECHA-LOG(5:1)
           MOVE WK-FECHA-SIS(5:2) TO FECHA-LOG(6:2)
           MOVE "/"               TO FECHA-LOG(8:1)
           MOVE WK-FECHA-SIS(7:2) TO FECHA-LOG(9:2)
           MOVE SPACE             TO FECHA-LOG(11:)

           MOVE WK-HORA-SIS(1:2) TO HORA-LOG(1:2)
           MOVE ":"              TO HORA-LOG(3:1)
           MOVE WK-HORA-SIS(3:2) TO HORA-LOG(4:2)
           MOVE ":"              TO HORA-LOG(6:1)
           MOVE WK-HORA-SIS(5:2) TO HORA-LOG(7:2)
           MOVE ":"              TO HORA-LOG(9:1)
           MOVE WK-HORA-SIS(7:2) TO HORA-LOG(10:2)
           MOVE SPACE            TO HORA-LOG(12:)

           MOVE WK-MSN-LOG  TO MENSAJE

           WRITE REG-LOG END-WRITE.


       0100-CERRAR-ARCHIVOS.
           CLOSE CUENTAS DATOS-TXT ARCHIVO-LOG.

       END PROGRAM DatosBatch.
