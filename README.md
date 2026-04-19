# Reto técnico.
Se presenta la solución de la prueba técnica desarrollada en COBOL simulando un proceso Batch y online

## Programa: DatosBatch.cbl. 
Procesa un archivo secuencial de manera diaria, con registros de transacciones (D: deposito y W: retiros) para grabar 
o actualizar los datos de la entidad.

# Programa: ConsultarCuenta.cbl. 
Programa que simula una consulta online (CICS) utilizando un archivo indexado. Solicita el número
de cuenta y valida que sea distinto de espacios. Si existe, presenta un sencillo menú
con tres opciones. Dependiendo de la opción ingresada, presenta la consulta o realiza 
proceso de retiro validando que el monto no supere el saldo disponible.

# Tecnologías utilizadas. 
- COBOL (GnuCOBOL).
- Archivo indexado para consultar y grabar registros.
- Archivos secuenciales para lectura secuencial, grabar historial y datos de salida.

# Estructura del proyecto. 
- Carpeta bin con los ejecutables generados.
- Documentación con imagenes de prueba de cada programa.
- Archivo indexado CUENTAS.
- Programa DatosBatch.cbl y ConsultarCuenta.cbl.
- Archivo de entrada DATOS:TXT.
- Archivo de salida con resumen del proceso.
- Archivo LOG.TXT con el historial de ejecución.

# Conceptos aplicados.
- Flujo tipo batch y procesamiento de registros.
- Manejo de archivos y FILE STATUS.
- Manejo de lectura.
- Validaciones y lógica orientada al negocio.
