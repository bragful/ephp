<?php
function manejador_excepciones($excepción) {
  echo "Excepción no capturada: " , $excepción->getMessage(), "\n";
}

set_exception_handler('manejador_excepciones');
set_exception_handler('other');
var_dump(restore_exception_handler());

throw new Exception('Excepción No Capturada');
echo "No Ejecutado\n";
