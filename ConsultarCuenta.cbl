      ******************************************************************
      * Author: Carlos Fernando Camacho Agón
      * Date: 2026/04/18
      * Purpose: Realizar consulta o retiro de una cuenta por medio
      *          de su llave única
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ConsultarCuenta.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----Select para archivo Indexado
           SELECT CUENTAS
               ASSIGN TO "C:\Users\USUARIO\Pr_Tecnica\CUENTAS"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS LV0-CTA
               FILE STATUS  IS ST-CTA.

       DATA DIVISION.
       FILE SECTION.
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

       WORKING-STORAGE SECTION.
       01  WK-VARIABLES.
           02 SW           PIC 9.
           02 WK-VLR       PIC $ZZZ,ZZZ,ZZZ,ZZZ.99.
           02 WK-OPC       PIC 9.
           02 ST-CTA       PIC XX.
           02 WK-SALDO     PIC $ZZZ,ZZZ,ZZZ,ZZZ.99.
           02 WK-VLR-TEM   PIC 9(12).
           02 WK-NRO-CTA   PIC X(06).
           02 WK-SEGUNDOS  PIC 9 VALUE 2.
           02 WK-VLR-SALDO PIC S9(12).

       PROCEDURE DIVISION.
       DECLARATIVES.

       ERROR-DATOS SECTION.
       USE AFTER ERROR PROCEDURE ON CUENTAS.

       ERROR-CUENTAS-LOGIC.
       IF  ST-CTA = "35"
           OPEN OUTPUT CUENTAS
           CLOSE CUENTAS
           OPEN I-O CUENTAS
       END-IF.
       END DECLARATIVES.

       MAIN-PROCEDURE.
           PERFORM 0010-TOMAR-DATOS

           IF SW = 0
               OPEN I-O CUENTAS
               PERFORM 0025-CONTINUA-PROCESO
               CLOSE CUENTAS
           END-IF

           DISPLAY "==================================================="
           DISPLAY "             FIN DEL PROCESO BANCARIO              "
           DISPLAY "==================================================="

           STOP RUN.

       0010-TOMAR-DATOS.
           DISPLAY "==================================================="
           DISPLAY "          BIENVENIDO AL SISTEMA BANCARIO           "
           DISPLAY "==================================================="

           DISPLAY "Por favor ingrese el numero de cuenta: "
           ACCEPT WK-NRO-CTA

           IF WK-NRO-CTA EQUAL SPACES
               MOVE 1 TO SW
               DISPLAY "La cuenta no puede ser vacia"
           ELSE
               PERFORM 0015-TOMAR-OPC
           END-IF.

       0015-TOMAR-OPC.
           DISPLAY "Seleccione una opcion: "
           DISPLAY "1 - Consultar cuenta"
           DISPLAY "2 - Realizar Retiro"
           DISPLAY "3 - Salir"
           ACCEPT WK-OPC

           EVALUATE WK-OPC
               WHEN 1 CONTINUE
               WHEN 2
                   PERFORM 0020-TOMAR-VLR
               WHEN 3
                   MOVE 1 TO SW
               WHEN OTHER
                   MOVE 1 TO SW
                   DISPLAY "Opcion no valida"
           END-EVALUATE.

       0020-TOMAR-VLR.
           DISPLAY "INGRESE EL VALOR A RETIRAR: "
           ACCEPT WK-VLR-TEM

           IF WK-VLR-TEM < 1
               MOVE 1 TO SW
               DISPLAY "Error en el valor ingresado"
           END-IF.

       0025-CONTINUA-PROCESO.
           MOVE WK-NRO-CTA TO LV0-CTA

           READ CUENTAS WITH NO LOCK KEY IS LV0-CTA INVALID KEY
               DISPLAY "<<<<<<<<<<<<<<<<<<ERROR>>>>>>>>>>>>>>>>>>>>>>>>"
               DISPLAY "La cuenta " LV0-CTA " no existe"
           NOT INVALID KEY
               INITIALIZE WK-VLR WK-SALDO
               IF WK-OPC = 1
                   MOVE VLR-SALDO TO  WK-VLR
                   DISPLAY "==========================================="
                   DISPLAY "Numero de cuenta: " LV0-CTA
                   DISPLAY "Saldo Disponible: " WK-VLR
                   DISPLAY "==========================================="
               ELSE
                   PERFORM 0030-REALIZAR-MOV
               END-IF
           END-READ.

       0030-REALIZAR-MOV.
           IF WK-VLR-TEM > VLR-SALDO
               MOVE WK-VLR-TEM TO WK-VLR
               MOVE VLR-SALDO  TO WK-SALDO
               DISPLAY "<<<<<<<<<<<<<<<<<<ERROR>>>>>>>>>>>>>>>>>>>>>>>>"
               DISPLAY "Actualmente su saldo es: " WK-SALDO
               DISPLAY "Y el monto ingresado es: " WK-VLR
               DISPLAY "<<<<<<<<<IMPOSIBLE REALIZAR EL RETIRO>>>>>>>>>>"
           ELSE
               INITIALIZE WK-VLR-SALDO
               COMPUTE WK-VLR-SALDO = VLR-SALDO - WK-VLR-TEM
               MOVE WK-VLR-SALDO   TO VLR-SALDO
               REWRITE REG-CUENTAS INVALID KEY
                   DISPLAY "==========================================="
                   DISPLAY "****OCURRIO UN ERROR. VUELVA A INTENTAR****"
                   DISPLAY "==========================================="
               NOT INVALID KEY
                   DISPLAY "==========================================="
                   DISPLAY "********PROCESO REALIZADO CON EXITO********"
                   DISPLAY "==========================================="
                   CALL "C$SLEEP" USING WK-SEGUNDOS
                   MOVE VLR-SALDO    TO WK-VLR
                   DISPLAY "SU NUEVO SALDO ES: " WK-VLR
               END-REWRITE
           END-IF.
       END PROGRAM ConsultarCuenta.
