<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<!-- MMM
<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>
MMM -->

<h1>[EN] Welcome to adea project!</h1>

<p>Data Envelopment Analysis (DEA) seeks to compare the relative efficiency of a group's decision units (DMU).</p>

<p>This package provides additional features to support variable selection, restricted DEA models, and leverage analysis.</p>

<p>The selection of variables in DEA is a cornerstone, because efficiency results hardly depend on that selection. A measured, called load, allows handling that selection. In the package to do it two step-by-step functions are provided.</p>

<p>Sometimes the reasons for external analysis require that the contributions of variables to the DEA be restricted in a range. Another function is also provided for this purpose.</p>

<p>Another feature provided allows us to find which DMUs cause the most significant changes in the results so that we can handle them properly.</p>

<p>More information in <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>r-forge project summary page</strong></a> or in <a href="http://knuth.uca.es/moodle/course/view.php?id=74&lang=en">project page</a>.</p>

<p><b>References:</b>
<ul>
<li><a href="http://dx.doi.org/10.1590/0101-7438.2018.038.01.0031">Stepwise selection of variables in DEA using contribution loads. <i>Fernando Fernandez-Palacin, Maria Auxiliadora Lopez-Sanchez, Manuel Munoz-Marquez</i>. Pesquisa Operacional, <b>38</b>:1, pg. 31-52. 2018, DOI: 10.1590/0101-7438.2018.038.01.0031.</a></li>
<li><a href="https://doi.org/10.1016/j.ejor.2020.08.021">Methodology for calculating critical values of relevance measures in variable selection methods in data envelopment analysis. <i>Jeyms Villanueva-Cantillo, Manuel Munoz-Marquez</i>. European Journal of Operational Research, 20:2, 627-670, 2021. DOI: 10.1016/j.ejor.2020.08.021.</a></li>
</ul>
</p>

<h1>[ES] Bienvenido al proyecto adea!</h1>

<p>El análisis envolvente de datos (DEA) busca comparar la eficiencia relativa de un grupo de unidades de decisión (DMU). </p>

<p>Este paquete proporciona funciones adicionales para la selección de variables, modelos DEA restringidos y análisis de influencia.</p>

<p>La selección de variables en DEA es una piedra angular, porque los resultados de eficiencia dependen de manera importante de esa selección. Una medida, llamada carga, permite realizar esa selección. 
El paquete proporciona dos funciones para hacerlo paso a paso. </p>

<p>A veces, razones externas al análisis requieren que las contribuciones de las variables al DEA se restrinjan en un rango. También se proporciona otra función para este propósito.</p>

<p> Otra función proporcionada permite encontrar qué DMUs causan los cambios más significativos en los resultados para tratarlas adecuadamente.</p>

<p>Más información en <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>r-forge project summary page</strong></a> or in <a href="http://knuth.uca.es/moodle/course/view.php?id=74&lang=es">project page</a>.</p>

<p><b>Referencias:</b>
<ul>
<li><a href="http://dx.doi.org/10.1590/0101-7438.2018.038.01.0031">Stepwise selection of variables in DEA using contribution loads. <i>Fernando Fernandez-Palacin, Maria Auxiliadora Lopez-Sanchez, Manuel Munoz-Marquez</i>. Pesquisa Operacional, <b>38</b>:1, pg. 31-52. 2018, DOI: 10.1590/0101-7438.2018.038.01.0031.</a></li>
<li><a href="https://doi.org/10.1016/j.ejor.2020.08.021">Methodology for calculating critical values of relevance measures in variable selection methods in data envelopment analysis. <i>Jeyms Villanueva-Cantillo, Manuel Munoz-Marquez</i>. European Journal of Operational Research, 20:2, 627-670, 2021. DOI: 10.1016/j.ejor.2020.08.021.</a></li>
</ul>
</p>

</body>
</html>
