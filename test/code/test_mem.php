<?php
// Reproducción: objetos anidados en arrays liberados prematuramente en ephp.
// En PHP real (Zend), la salida NO debe mostrar ningún __destruct hasta el
// bloque FINAL. En ephp, con el bug, los destructores se disparan en los
// puntos marcados y los accesos posteriores fallan o devuelven basura
// (segmentation_fault o un objeto de otra clase si el slot fue reutilizado).

class Probe {
    public $tag;
    function __construct($tag) { $this->tag = $tag; echo "construct($tag)\n"; }
    function hello() { return "hello from {$this->tag}"; }
    function __destruct() { echo "destruct({$this->tag})\n"; }
}

echo "== CASO 1: copia de array por asignacion ==\n";
$a = ['obj' => new Probe('caso1')];
$b = $a;            // copia del array; el objeto interno deberia ganar un link
unset($b);          // BUG: destroy_data recursivo decrementa a 0 -> destruct
echo $a['obj']->hello(), "\n";   // BUG: use-after-free aqui
echo "-- fin caso 1 (no debe haber destruct antes de esta linea)\n\n";

echo "== CASO 2: paso de array como argumento por valor ==\n";
function consume(array $arr) {
    echo $arr['obj']->hello(), " (dentro de la funcion)\n";
    // al salir, el scope de la funcion se destruye:
    // BUG: decrementa el objeto interno aunque el caller aun lo tiene
}
$c = ['obj' => new Probe('caso2')];
consume($c);
echo $c['obj']->hello(), "\n";   // BUG: use-after-free aqui
echo "-- fin caso 2\n\n";

echo "== CASO 3: return de array con objetos ==\n";
function produce() {
    $local = ['obj' => new Probe('caso3')];
    return $local;   // BUG: solo se protege el valor top-level;
                     // el destroy del scope mata el objeto interno
}
$d = produce();
echo $d['obj']->hello(), "\n";   // BUG: use-after-free aqui
echo "-- fin caso 3\n\n";

echo "== CASO 4: reutilizacion de slot (corrupcion silenciosa) ==\n";
class Impostor {
    function hello() { return "SOY EL IMPOSTOR"; }
    function __destruct() { echo "destruct(impostor)\n"; }
}
$e = ['obj' => new Probe('caso4')];
$f = $e;
unset($f);                  // BUG: libera el slot del objeto de $e
$g = new Impostor();        // reutiliza el slot mas bajo libre
// Si sale "SOY EL IMPOSTOR", el obj_ref colgante apunta al nuevo objeto:
echo $e['obj']->hello(), "\n";
echo "-- fin caso 4\n\n";

echo "== FINAL: aqui es donde deben aparecer TODOS los destruct ==\n";
