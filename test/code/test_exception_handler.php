<?php
function manejador_excepciones($excepción) {
  echo "Excepción no capturada: " , $excepción->getMessage(), "\n";
}

set_exception_handler('manejador_excepciones');

throw new Exception('Excepción No Capturada', 0);
echo "No Ejecutado\n";
