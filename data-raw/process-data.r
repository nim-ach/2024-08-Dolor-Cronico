# Clear existing data and graphics
rm(list=ls())
graphics.off()

# Load libraries
library(data.table)

# Read Data
data=fread('data-raw/raw-data.csv')

# Setting Factors(will create new variable for factors)
data$genero = factor(data$genero,levels=c("1","2","3","4"))
data$nacionalidad[data$nacionalidad == ""] = NA
data$nacionalidad = factor(data$nacionalidad)
data$universidad = factor(data$universidad,levels=c("1","2"))
data$area_estudios = fcase(
  grepl("kine|enferme|terapia|medic|obster|fonoaud|nutric", tolower(data$carrera_universitaria)), "Ciencias de la Salud",
  grepl("social|psicolog|derecho|pedadog|turismo", tolower(data$carrera_universitaria)), "Ciencias Sociales",
  grepl("ingenie|arquitec|agronom", tolower(data$carrera_universitaria)), "Ciencias de la Ingeniería",
  grepl("contador|administraci", tolower(data$carrera_universitaria)), "Ciencias Económicas y Administrativas"
) |> as.factor()
data$horario = factor(data$horario,levels=c("1","2"))
data$condicion_laboral___1 = factor(data$condicion_laboral___1,levels=c("0","1"))
data$condicion_laboral___2 = factor(data$condicion_laboral___2,levels=c("0","1"))
data$condicion_laboral___3 = factor(data$condicion_laboral___3,levels=c("0","1"))
data$condicion_laboral___4 = factor(data$condicion_laboral___4,levels=c("0","1"))
data$condicion_laboral___5 = factor(data$condicion_laboral___5,levels=c("0","1"))
data$consumo_alcohol = factor(data$consumo_alcohol,levels=c("1","2"))
data$consume_tabaco = factor(data$consume_tabaco,levels=c("1","2"))
data$consumo_drogas = factor(data$consumo_drogas,levels=c("1","2"))
data$diagnostico_de_cancer = factor(data$diagnostico_de_cancer,levels=c("1","2"))
data$dolor_frecuente_3meses = factor(data$dolor_frecuente_3meses,levels=c("1","2"))
data$localizacion_dolor___1 = factor(data$localizacion_dolor___1,levels=c("0","1"))
data$localizacion_dolor___2 = factor(data$localizacion_dolor___2,levels=c("0","1"))
data$localizacion_dolor___3 = factor(data$localizacion_dolor___3,levels=c("0","1"))
data$localizacion_dolor___4 = factor(data$localizacion_dolor___4,levels=c("0","1"))
data$localizacion_dolor___5 = factor(data$localizacion_dolor___5,levels=c("0","1"))
data$localizacion_dolor___6 = factor(data$localizacion_dolor___6,levels=c("0","1"))
data$localizacion_dolor___7 = factor(data$localizacion_dolor___7,levels=c("0","1"))
data$localizacion_dolor___8 = factor(data$localizacion_dolor___8,levels=c("0","1"))
data$localizacion_dolor___9 = factor(data$localizacion_dolor___9,levels=c("0","1"))
data$localizacion_dolor___10 = factor(data$localizacion_dolor___10,levels=c("0","1"))
data$localizacion_dolor___11 = factor(data$localizacion_dolor___11,levels=c("0","1"))
data$localizacion_dolor___12 = factor(data$localizacion_dolor___12,levels=c("0","1"))
data$localizacion_dolor___13 = factor(data$localizacion_dolor___13,levels=c("0","1"))
data$localizacion_dolor___14 = factor(data$localizacion_dolor___14,levels=c("0","1"))
data$localizacion_dolor___15 = factor(data$localizacion_dolor___15,levels=c("0","1"))
data$localizacion_dolor___16 = factor(data$localizacion_dolor___16,levels=c("0","1"))
data$localizacion_dolor___17 = factor(data$localizacion_dolor___17,levels=c("0","1"))
data$frecuencia_dolor = fcase(
  data$frecuencia_dolor___1 == 1, 5,
  data$frecuencia_dolor___2 == 1, 4,
  data$frecuencia_dolor___3 == 1, 3,
  data$frecuencia_dolor___4 == 1, 2,
  data$frecuencia_dolor___5 == 1, 1)
data$frecuencia_dolor___1 <-
  data$frecuencia_dolor___2 <-
  data$frecuencia_dolor___3 <-
  data$frecuencia_dolor___4 <-
  data$frecuencia_dolor___5 <- NULL
data$frecuencia_dolor = factor(data$frecuencia_dolor, levels = c(1:5))
data$consulta_respecto_a_dolor___1 = factor(data$consulta_respecto_a_dolor___1,levels=c("0","1"))
data$consulta_respecto_a_dolor___2 = factor(data$consulta_respecto_a_dolor___2,levels=c("0","1"))
data$consulta_respecto_a_dolor___3 = factor(data$consulta_respecto_a_dolor___3,levels=c("0","1"))
data$consulta_respecto_a_dolor___4 = factor(data$consulta_respecto_a_dolor___4,levels=c("0","1"))
data$consulta_respecto_a_dolor___5 = factor(data$consulta_respecto_a_dolor___5,levels=c("0","1"))
data$consulta_respecto_a_dolor___6 = factor(data$consulta_respecto_a_dolor___6,levels=c("0","1"))
data$consulta_respecto_a_dolor___7 = factor(data$consulta_respecto_a_dolor___7,levels=c("0","1"))
data$consulta_respecto_a_dolor___8 = factor(data$consulta_respecto_a_dolor___8,levels=c("0","1"))
data$consulta_respecto_a_dolor___9 = factor(data$consulta_respecto_a_dolor___9,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___1 = factor(data$enfermedad_que_provoca_dolor___1,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___2 = factor(data$enfermedad_que_provoca_dolor___2,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___3 = factor(data$enfermedad_que_provoca_dolor___3,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___4 = factor(data$enfermedad_que_provoca_dolor___4,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___5 = factor(data$enfermedad_que_provoca_dolor___5,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___6 = factor(data$enfermedad_que_provoca_dolor___6,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___7 = factor(data$enfermedad_que_provoca_dolor___7,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___8 = factor(data$enfermedad_que_provoca_dolor___8,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___9 = factor(data$enfermedad_que_provoca_dolor___9,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___10 = factor(data$enfermedad_que_provoca_dolor___10,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___11 = factor(data$enfermedad_que_provoca_dolor___11,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___12 = factor(data$enfermedad_que_provoca_dolor___12,levels=c("0","1"))
data$enfermedad_que_provoca_dolor___13 = factor(data$enfermedad_que_provoca_dolor___13,levels=c("0","1"))
data$caracterizacion_de_dolor___1 = factor(data$caracterizacion_de_dolor___1,levels=c("0","1"))
data$caracterizacion_de_dolor___2 = factor(data$caracterizacion_de_dolor___2,levels=c("0","1"))
data$caracterizacion_de_dolor___3 = factor(data$caracterizacion_de_dolor___3,levels=c("0","1"))
data$caracterizacion_de_dolor___4 = factor(data$caracterizacion_de_dolor___4,levels=c("0","1"))
data$caracterizacion_de_dolor___5 = factor(data$caracterizacion_de_dolor___5,levels=c("0","1"))
data$caracterizacion_de_dolor___6 = factor(data$caracterizacion_de_dolor___6,levels=c("0","1"))
data$identifique_su_dolor___1 = factor(data$identifique_su_dolor___1,levels=c("0","1"))
data$identifique_su_dolor___2 = factor(data$identifique_su_dolor___2,levels=c("0","1"))
data$identifique_su_dolor___3 = factor(data$identifique_su_dolor___3,levels=c("0","1"))
data$identifique_su_dolor___4 = factor(data$identifique_su_dolor___4,levels=c("0","1"))
data$identifique_su_dolor___5 = factor(data$identifique_su_dolor___5,levels=c("0","1"))
data$identifique_su_dolor___6 = factor(data$identifique_su_dolor___6,levels=c("0","1"))
data$identifique_su_dolor___7 = factor(data$identifique_su_dolor___7,levels=c("0","1"))
data$identifique_su_dolor___8 = factor(data$identifique_su_dolor___8,levels=c("0","1"))
data$identifique_su_dolor___9 = factor(data$identifique_su_dolor___9,levels=c("0","1"))
data$frecuencia_medicamentos___1 = factor(data$frecuencia_medicamentos___1,levels=c("0","1"))
data$frecuencia_medicamentos___2 = factor(data$frecuencia_medicamentos___2,levels=c("0","1"))
data$frecuencia_medicamentos___3 = factor(data$frecuencia_medicamentos___3,levels=c("0","1"))
data$frecuencia_medicamentos___4 = factor(data$frecuencia_medicamentos___4,levels=c("0","1"))
data$frecuencia_medicamentos___5 = factor(data$frecuencia_medicamentos___5,levels=c("0","1"))
data$ttos_para_el_dolor___1 = factor(data$ttos_para_el_dolor___1,levels=c("0","1"))
data$ttos_para_el_dolor___2 = factor(data$ttos_para_el_dolor___2,levels=c("0","1"))
data$ttos_para_el_dolor___3 = factor(data$ttos_para_el_dolor___3,levels=c("0","1"))
data$ttos_para_el_dolor___4 = factor(data$ttos_para_el_dolor___4,levels=c("0","1"))
data$ttos_para_el_dolor___5 = factor(data$ttos_para_el_dolor___5,levels=c("0","1"))
data$ttos_para_el_dolor___6 = factor(data$ttos_para_el_dolor___6,levels=c("0","1"))
data$ttos_para_el_dolor___7 = factor(data$ttos_para_el_dolor___7,levels=c("0","1"))
data$ttos_para_el_dolor___8 = factor(data$ttos_para_el_dolor___8,levels=c("0","1"))
data$ttos_para_el_dolor___9 = factor(data$ttos_para_el_dolor___9,levels=c("0","1"))
data$cuanto_afecta_dolor = as.numeric(data$cuanto_afecta_dolor)
data$afecto_autocuidado = as.numeric(data$afecto_autocuidado)
data$afecta_caminar = as.numeric(data$afecta_caminar)
data$afecta_trabajo = as.numeric(data$afecta_trabajo)
data$afecta_act_sociales = as.numeric(data$afecta_act_sociales)
data$afecta_animo = as.numeric(data$afecta_animo)
data$afecta_triste = as.numeric(data$afecta_triste)
data$afecta_vida_sexual = as.numeric(data$afecta_vida_sexual)
data$afecta_suenno = as.numeric(data$afecta_suenno)
data$indicacion_medicamentos___1 = factor(data$indicacion_medicamentos___1,levels=c("0","1"))
data$indicacion_medicamentos___2 = factor(data$indicacion_medicamentos___2,levels=c("0","1"))
data$indicacion_medicamentos___3 = factor(data$indicacion_medicamentos___3,levels=c("0","1"))
data$indicacion_medicamentos___4 = factor(data$indicacion_medicamentos___4,levels=c("0","1"))
data$indicacion_medicamentos___5 = factor(data$indicacion_medicamentos___5,levels=c("0","1"))
data$indicacion_medicamentos___6 = factor(data$indicacion_medicamentos___6,levels=c("0","1"))
data$indicacion_medicamentos___7 = factor(data$indicacion_medicamentos___7,levels=c("0","1"))
data$indicacion_medicamentos___8 = factor(data$indicacion_medicamentos___8,levels=c("0","1"))
data$ttos_manejo_dolor___1 = factor(data$ttos_manejo_dolor___1,levels=c("0","1"))
data$ttos_manejo_dolor___2 = factor(data$ttos_manejo_dolor___2,levels=c("0","1"))
data$ttos_manejo_dolor___3 = factor(data$ttos_manejo_dolor___3,levels=c("0","1"))
data$ttos_manejo_dolor___4 = factor(data$ttos_manejo_dolor___4,levels=c("0","1"))
data$ttos_manejo_dolor___5 = factor(data$ttos_manejo_dolor___5,levels=c("0","1"))
data$ttos_manejo_dolor___6 = factor(data$ttos_manejo_dolor___6,levels=c("0","1"))
data$ttos_manejo_dolor___7 = factor(data$ttos_manejo_dolor___7,levels=c("0","1"))
data$licencia_por_dolor = factor(data$licencia_por_dolor,levels=c("1","2"))
data$mets_vigorosa =
  8.0 * data$durante_los_ltimos_7_d_as * data$habitualmente_cu_nto_tiemp
data$mets_moderada =
  4.0 * data$los_ltimos_7_d_as * data$dias_moderada
data$mets_caminar =
  3.3 * data$dias_caminata * as.numeric(data$tiempo_caminata)
data$mets_total = data$mets_vigorosa + data$mets_moderada + data$mets_caminar
data$score_ipaq = factor(data$score_ipaq,levels=c("1","2","3"))
data$matutinidad_q1 = factor(data$matutinidad_q1,levels=c("5","4","3","2","1"))
data$matutinidad_q2 = factor(data$matutinidad_q2,levels=c("5","4","3","2","1"))
data$matutinidad_q3 = factor(data$matutinidad_q3,levels=c("4","3","2","1"))
data$matutinidad_q4 = factor(data$matutinidad_q4,levels=c("1","2","3","4"))
data$matutinidad_q5 = factor(data$matutinidad_q5,levels=c("1","2","3","4"))
data$matutinidad_q6 = factor(data$matutinidad_q6,levels=c("1","2","3","4"))
data$matutinidad_q7 = factor(data$matutinidad_q7,levels=c("1","2","3","4"))
data$matutinidad_q8 = factor(data$matutinidad_q8,levels=c("4","3","2","1"))
data$matutinidad_q9 = factor(data$matutinidad_q9,levels=c("4","3","2","1"))
data$matutinidad_q10 = factor(data$matutinidad_q10,levels=c("5","4","3","2","1"))
data$matutinidad_q11 = factor(data$matutinidad_q11,levels=c("6","4","2","0"))
data$matutinidad_q12 = factor(data$matutinidad_q12,levels=c("0","2","3","5"))
data$matutinidad_q13 = factor(data$matutinidad_q13,levels=c("4","3","2","1"))
data$matutinidad_q14 = factor(data$matutinidad_q14,levels=c("1","2","3","4"))
data$matutinidad_q15 = factor(data$matutinidad_q15,levels=c("4","3","2","1"))
data$matutinidad_q16 = factor(data$matutinidad_q16,levels=c("1","2","3","4"))
data$matutinidad_q17 = factor(data$matutinidad_q17,levels=c("5","4","3","2","1"))
data$matutinidad_q18 = factor(data$matutinidad_q18,levels=c("5","4","3","2","1"))
data$matutinidad_q19 = factor(data$matutinidad_q19,levels=c("6","4","2","0"))
data$score_matutinidad = factor(data$score_matutinidad,levels=c("1","2","3","4","5"))
data$pittsburg_pje_q2 = factor(data$pittsburg_pje_q2,levels=c("0","1","2","3"))
data$calificacion_pittsburg_2 = factor(data$calificacion_pittsburg_2,levels=c("0","1","2","3"))
data$pittsburg_pje_q4 = factor(data$pittsburg_pje_q4,levels=c("0","1","2","3"))
data$pittsburg_q5a = factor(data$pittsburg_q5a,levels=c("0","1","2","3"))
data$pittsburg_q5b = factor(data$pittsburg_q5b,levels=c("0","1","2","3"))
data$pittsburg_q5c = factor(data$pittsburg_q5c,levels=c("0","1","2","3"))
data$pittsburg_q5d = factor(data$pittsburg_q5d,levels=c("0","1","2","3"))
data$pittsburg_q5e = factor(data$pittsburg_q5e,levels=c("0","1","2","3"))
data$pittsburg_q5f = factor(data$pittsburg_q5f,levels=c("0","1","2","3"))
data$pittsburg_q5g = factor(data$pittsburg_q5g,levels=c("0","1","2","3"))
data$pittsburg_q5h = factor(data$pittsburg_q5h,levels=c("0","1","2","3"))
data$pittsburg_q5i = factor(data$pittsburg_q5i,levels=c("0","1","2","3"))
data$pittsburg_q6 = factor(data$pittsburg_q6,levels=c("0","1","2","3"))
data$pittsburg_q7 = factor(data$pittsburg_q7,levels=c("0","1","2","3"))
data$pittsburg_q8 = factor(data$pittsburg_q8,levels=c("0","1","2","3"))
data$pittsburg_q9 = factor(data$pittsburg_q9,levels=c("0","1","2","3"))
data$puntaje_latencia_del_sueno = factor(data$puntaje_latencia_del_sueno,levels=c("0","1","2","3"))
data$score_componente4 = factor(data$score_componente4,levels=c("0","1","2","3"))
data$categorizacion_comp5 = factor(data$categorizacion_comp5,levels=c("0","1","2","3"))
data$score_disf_nocturna = factor(data$score_disf_nocturna,levels=c("0","1","2","3"))
data$realizacion_examenes = factor(data$realizacion_examenes,levels=c("1","2","3","4","5"))
data$exposicion_de_trabajos = factor(data$exposicion_de_trabajos,levels=c("1","2","3","4","5"))
data$intervencion_clases = factor(data$intervencion_clases,levels=c("1","2","3","4","5"))
data$tratar_profesor = factor(data$tratar_profesor,levels=c("1","2","3","4","5"))
data$sobrecarga_academica = factor(data$sobrecarga_academica,levels=c("1","2","3","4","5"))
data$falta_de_tiempo = factor(data$falta_de_tiempo,levels=c("1","2","3","4","5"))
data$competitividad_companeros = factor(data$competitividad_companeros,levels=c("1","2","3","4","5"))
data$realizar_trabajos = factor(data$realizar_trabajos,levels=c("1","2","3","4","5"))
data$la_tarea_de_estudiar = factor(data$la_tarea_de_estudiar,levels=c("1","2","3","4","5"))
data$problemas_con_profesores = factor(data$problemas_con_profesores,levels=c("1","2","3","4","5"))
data$problemas_companeros = factor(data$problemas_companeros,levels=c("1","2","3","4","5"))
data$asistencia_clases = factor(data$asistencia_clases,levels=c("1","2","3","4","5"))
data$exceso_de_responsabilidad = factor(data$exceso_de_responsabilidad,levels=c("1","2","3","4","5"))
data$obtener_notas_elevadas = factor(data$obtener_notas_elevadas,levels=c("1","2","3","4","5"))
data$perspectivas_profesionales = factor(data$perspectivas_profesionales,levels=c("1","2","3","4","5"))
data$eleccion_asignaturas = factor(data$eleccion_asignaturas,levels=c("1","2","3","4","5"))
data$mantener_beca = factor(data$mantener_beca,levels=c("1","2","3","4","5"))
data$acabar_los_estudios = factor(data$acabar_los_estudios,levels=c("1","2","3","4","5"))
data$presion_familiar = factor(data$presion_familiar,levels=c("1","2","3","4","5"))
data$ansiedadbeck_q1 = factor(data$ansiedadbeck_q1,levels=c("0","1","2","3"))
data$ansiedadbeck_q2 = factor(data$ansiedadbeck_q2,levels=c("0","1","2","3"))
data$ansiedadbeck_q3 = factor(data$ansiedadbeck_q3,levels=c("0","1","2","3"))
data$ansiedadbeck_q4 = factor(data$ansiedadbeck_q4,levels=c("0","1","2","3"))
data$ansiedadbeck_q5 = factor(data$ansiedadbeck_q5,levels=c("0","1","2","3"))
data$ansiedadbeck_q6 = factor(data$ansiedadbeck_q6,levels=c("0","1","2","3"))
data$ansiedadbeck_q7 = factor(data$ansiedadbeck_q7,levels=c("0","1","2","3"))
data$ansiedadbeck_q8 = factor(data$ansiedadbeck_q8,levels=c("0","1","2","3"))
data$ansiedadbeck_q9 = factor(data$ansiedadbeck_q9,levels=c("0","1","2","3"))
data$ansiedadbeck_q10 = factor(data$ansiedadbeck_q10,levels=c("0","1","2","3"))
data$ansiedadbeck_q11 = factor(data$ansiedadbeck_q11,levels=c("0","1","2","3"))
data$ansiedadbeck_q12 = factor(data$ansiedadbeck_q12,levels=c("0","1","2","3"))
data$ansiedadbeck_q13 = factor(data$ansiedadbeck_q13,levels=c("0","1","2","3"))
data$ansiedadbeck_q14 = factor(data$ansiedadbeck_q14,levels=c("0","1","2","3"))
data$ansiedadbeck_q15 = factor(data$ansiedadbeck_q15,levels=c("0","1","2","3"))
data$ansiedadbeck_q16 = factor(data$ansiedadbeck_q16,levels=c("0","1","2","3"))
data$ansiedadbeck_q17 = factor(data$ansiedadbeck_q17,levels=c("0","1","2","3"))
data$ansiedadbeck_q18 = factor(data$ansiedadbeck_q18,levels=c("0","1","2","3"))
data$ansiedadbeck_q19 = factor(data$ansiedadbeck_q19,levels=c("0","1","2","3"))
data$ansiedadbeck_q20 = factor(data$ansiedadbeck_q20,levels=c("0","1","2","3"))
data$ansiedadbeck_q21 = factor(data$ansiedadbeck_q21,levels=c("0","1","2","3"))
data$categorizacion_ans_beck = factor(data$categorizacion_ans_beck,levels=c("0","1","2","3"))
data$dep_beck_q1 = factor(data$dep_beck_q1,levels=c("0","1","2","3"))
data$dep_beck_q2 = factor(data$dep_beck_q2,levels=c("0","1","2","3"))
data$dep_beck_q3 = factor(data$dep_beck_q3,levels=c("0","1","2","3"))
data$dep_beck_q4 = factor(data$dep_beck_q4,levels=c("0","1","2","3"))
data$dep_beck_q5 = factor(data$dep_beck_q5,levels=c("0","1","2","3"))
data$dep_beck_q6 = factor(data$dep_beck_q6,levels=c("0","1","2","3"))
data$dep_beck_q7 = factor(data$dep_beck_q7,levels=c("0","1","2","3"))
data$dep_beck_q8 = factor(data$dep_beck_q8,levels=c("0","1","2","3"))
data$dep_beck_q9 = factor(data$dep_beck_q9,levels=c("0","1","2","3"))
data$dep_beck_q10 = factor(data$dep_beck_q10,levels=c("0","1","2","3"))
data$dep_beck_q11 = factor(data$dep_beck_q11,levels=c("0","1","2","3"))
data$dep_beck_q12 = factor(data$dep_beck_q12,levels=c("0","1","2","3"))
data$dep_beck_q13 = factor(data$dep_beck_q13,levels=c("0","1","2","3"))
data$dep_beck_q14 = factor(data$dep_beck_q14,levels=c("0","1","2","3"))
data$dep_beck_q15 = factor(data$dep_beck_q15,levels=c("0","1","2","3"))
data$dep_beck_q16 = factor(data$dep_beck_q16,levels=c("0","1","2","3"))
data$dep_beck_q17 = factor(data$dep_beck_q17,levels=c("1","2","3","4"))
data$dep_beck_q18 = factor(data$dep_beck_q18,levels=c("0","1","2","3"))
data$dep_beck_q19 = factor(data$dep_beck_q19,levels=c("0","1","2","3"))
data$dep_beck_q20 = factor(data$dep_beck_q20,levels=c("0","1","2","3"))
data$dep_beck_q21 = factor(data$dep_beck_q21,levels=c("0","1","2","3"))
data$categorizacion_dep_beck = factor(data$categorizacion_dep_beck,levels=c("0","1","2","3"))

levels(data$genero)=c("Masculino","Femenino","Prefiero no responder","Otro")
levels(data$universidad)=c("Universidad de Magallanes","Universidad de Aysén")
levels(data$horario)=c("Diurno","Vespertino")
levels(data$condicion_laboral___1)=c("No","Sí")
levels(data$condicion_laboral___2)=c("No","Sí")
levels(data$condicion_laboral___3)=c("No","Sí")
levels(data$condicion_laboral___4)=c("No","Sí")
levels(data$condicion_laboral___5)=c("No","Sí")
levels(data$consumo_alcohol)=c("Sí","No")
levels(data$consume_tabaco)=c("Sí","No")
levels(data$consumo_drogas)=c("Sí","No")
levels(data$diagnostico_de_cancer)=c("Sí","No")
levels(data$dolor_frecuente_3meses)=c("Sí","No")
levels(data$localizacion_dolor___1)=c("No","Sí")
levels(data$localizacion_dolor___2)=c("No","Sí")
levels(data$localizacion_dolor___3)=c("No","Sí")
levels(data$localizacion_dolor___4)=c("No","Sí")
levels(data$localizacion_dolor___5)=c("No","Sí")
levels(data$localizacion_dolor___6)=c("No","Sí")
levels(data$localizacion_dolor___7)=c("No","Sí")
levels(data$localizacion_dolor___8)=c("No","Sí")
levels(data$localizacion_dolor___9)=c("No","Sí")
levels(data$localizacion_dolor___10)=c("No","Sí")
levels(data$localizacion_dolor___11)=c("No","Sí")
levels(data$localizacion_dolor___12)=c("No","Sí")
levels(data$localizacion_dolor___13)=c("No","Sí")
levels(data$localizacion_dolor___14)=c("No","Sí")
levels(data$localizacion_dolor___15)=c("No","Sí")
levels(data$localizacion_dolor___16)=c("No","Sí")
levels(data$localizacion_dolor___17)=c("No","Sí")
levels(data$frecuencia_dolor)=c("Al menos una vez por mes","Varias veces al mes", "Al menos una vez por semana", "Varias veces a la semana", "Todo el tiempo")
levels(data$consulta_respecto_a_dolor___1)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___2)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___3)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___4)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___5)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___6)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___7)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___8)=c("No","Sí")
levels(data$consulta_respecto_a_dolor___9)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___1)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___2)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___3)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___4)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___5)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___6)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___7)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___8)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___9)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___10)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___11)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___12)=c("No","Sí")
levels(data$enfermedad_que_provoca_dolor___13)=c("No","Sí")
levels(data$caracterizacion_de_dolor___1)=c("No","Sí")
levels(data$caracterizacion_de_dolor___2)=c("No","Sí")
levels(data$caracterizacion_de_dolor___3)=c("No","Sí")
levels(data$caracterizacion_de_dolor___4)=c("No","Sí")
levels(data$caracterizacion_de_dolor___5)=c("No","Sí")
levels(data$caracterizacion_de_dolor___6)=c("No","Sí")
levels(data$identifique_su_dolor___1)=c("No","Sí")
levels(data$identifique_su_dolor___2)=c("No","Sí")
levels(data$identifique_su_dolor___3)=c("No","Sí")
levels(data$identifique_su_dolor___4)=c("No","Sí")
levels(data$identifique_su_dolor___5)=c("No","Sí")
levels(data$identifique_su_dolor___6)=c("No","Sí")
levels(data$identifique_su_dolor___7)=c("No","Sí")
levels(data$identifique_su_dolor___8)=c("No","Sí")
levels(data$identifique_su_dolor___9)=c("No","Sí")
levels(data$frecuencia_medicamentos___1)=c("No","Sí")
levels(data$frecuencia_medicamentos___2)=c("No","Sí")
levels(data$frecuencia_medicamentos___3)=c("No","Sí")
levels(data$frecuencia_medicamentos___4)=c("No","Sí")
levels(data$frecuencia_medicamentos___5)=c("No","Sí")
levels(data$ttos_para_el_dolor___1)=c("No","Sí")
levels(data$ttos_para_el_dolor___2)=c("No","Sí")
levels(data$ttos_para_el_dolor___3)=c("No","Sí")
levels(data$ttos_para_el_dolor___4)=c("No","Sí")
levels(data$ttos_para_el_dolor___5)=c("No","Sí")
levels(data$ttos_para_el_dolor___6)=c("No","Sí")
levels(data$ttos_para_el_dolor___7)=c("No","Sí")
levels(data$ttos_para_el_dolor___8)=c("No","Sí")
levels(data$ttos_para_el_dolor___9)=c("No","Sí")
levels(data$indicacion_medicamentos___1)=c("No","Sí")
levels(data$indicacion_medicamentos___2)=c("No","Sí")
levels(data$indicacion_medicamentos___3)=c("No","Sí")
levels(data$indicacion_medicamentos___4)=c("No","Sí")
levels(data$indicacion_medicamentos___5)=c("No","Sí")
levels(data$indicacion_medicamentos___6)=c("No","Sí")
levels(data$indicacion_medicamentos___7)=c("No","Sí")
levels(data$indicacion_medicamentos___8)=c("No","Sí")
levels(data$ttos_manejo_dolor___1)=c("No","Sí")
levels(data$ttos_manejo_dolor___2)=c("No","Sí")
levels(data$ttos_manejo_dolor___3)=c("No","Sí")
levels(data$ttos_manejo_dolor___4)=c("No","Sí")
levels(data$ttos_manejo_dolor___5)=c("No","Sí")
levels(data$ttos_manejo_dolor___6)=c("No","Sí")
levels(data$ttos_manejo_dolor___7)=c("No","Sí")
levels(data$licencia_por_dolor)=c("Sí","No")
levels(data$score_ipaq)=c("Bajo","Moderado","Alto")
levels(data$matutinidad_q1)=c("Entre las 05:00 (5 AM) y 06: 30 (6:30 AM) de la mañana","Ente las 06:30 (6:30 AM) y las 07:45 (7:45 AM) de la mañana","Entre las 07:45 (7:45 AM) y las 09:45 (9:45 AM) de la mañana","Entre las 09:45 (9:45 AM) y las 11:00 (11 AM) de la mañana","Entre las 11 (11 AM) de la mañana y las 12 del día")
levels(data$matutinidad_q2)=c("A las 20:00 (8 PM) - 21:00 (9 PM)","A las 21:00 (9 PM) - 22:15 (10:15 PM)","A las 22:15 (10:15 PM) - 00:30 (12:30 AM)","A las 00:30 (12:30 AM) - 01:45 (1:45 AM)","A las 01:45 (1:45 AM) - 03:00 (3 AM)")
levels(data$matutinidad_q3)=c("No lo necesito","Lo necesito poco","Lo necesito bastante","Lo necesito mucho")
levels(data$matutinidad_q4)=c("Nada fácil","No muy fácil","Bastante fácil","Muy fácil")
levels(data$matutinidad_q5)=c("Nada alerta","Poco alerta","Bastante alerta","Muy alerta")
levels(data$matutinidad_q6)=c("Muy escaso","Bastante escaso","Bastante bueno","Muy bueno")
levels(data$matutinidad_q7)=c("Muy cansado","Bastante cansado","Bastante descansado","Muy descando")
levels(data$matutinidad_q8)=c("Nunca o raramente más tarde","Menos de 1 hora más tarde","De 1 a 2 horas más tarde","Más de 2 horas más tarde")
levels(data$matutinidad_q9)=c("Estaría en buena forma","Estaría en una forma aceptable","Me resultaria difícil","Me resultaría muy dificil")
levels(data$matutinidad_q10)=c("A las 20:00 (8 PM) - 21:00 (9 PM)","A las 21:00 (9 PM) - 22:15 (10:15 PM)","A las 22:15 (10:15 PM) - 00:45 (12:45 AM)","A las 00:45 (12:45 AM) - 02:00 (2 AM)","A las 02:00 (2 AM) - 03:00 (3 AM)")
levels(data$matutinidad_q11)=c("De 08:00 (8 AM) a 10:00 (10 AM)","De 11:00 (11 AM) a 13:00 (1 PM)","De 13:00 (1 PM) a 17:00 (5 PM)","De 19:00 (7 PM) a 21:00 (9 PM)")
levels(data$matutinidad_q12)=c("Ningún cansancio","Algún cansancio","Bastante cansancio","Mucho cansancio")
levels(data$matutinidad_q13)=c("A la hora habitual y ya no dormiría más","A la hora habitual y luego dormitaría","A la hora habitual y volvería a dormirme","Más tarde de lo habitual")
levels(data$matutinidad_q14)=c("No acostarme hasta pasada la guardia","Echar una siesta antes y dormir después","Echar un buen sueño antes y una siesta después","Sólo dormirías antes de la guardia")
levels(data$matutinidad_q15)=c("De 08:00 (8 AM) a 10:00 (10 AM)","De 11:00 (11 AM) a 13:00 (1 PM)","De 13:00 (1 PM) a 17:00 (5 PM)","De 19:00 (7 PM) a 21:00 (9 PM)")
levels(data$matutinidad_q16)=c("Estaría en buena forma","Estaría en una forma aceptable","Me resultaría difícil","Me resultaria muy difícil")
levels(data$matutinidad_q17)=c("Entre las 04:00 (4 AM) y las 08:00 (8 AM)","Entre las 08:00 (8 AM) y las 09:00 (9 AM)","Entre las 09:00 (9 AM) y las 14:00 (2 PM)","Entre las 14:00 (2 PM) y las 17:00 (5 PM)","Entre las 17:00 (5 PM) y las 04:00 (4 AM)")
levels(data$matutinidad_q18)=c("Entre las 05:00 (5 AM) y las 08:00 (8 AM)","Entre las 08:00 (8 AM) y las 10:00 (10 AM)","Entre las 10:00 (10 AM) y las 17:00 (5 PM)","Entre las 17:00 (5 PM) y las 22:00 (10 PM)","Entre las 22:00 (10 PM) y las 05:00 (5 AM)")
levels(data$matutinidad_q19)=c("Un tipo claramente matutino.","Un tipo más matutino que vespertino.","Un tipo más vespertino que matutino.","Un tipo claramente vespertino")
levels(data$score_matutinidad)=c("Vespertino extremo","Vespertino moderado","Intermedio","Matutino moderado","Matutino extremo")
levels(data$pittsburg_pje_q2)=c("< 15 minutos","16-30 minutos","31-60 minutos",">60 minutos")
levels(data$calificacion_pittsburg_2)=c("< 15 minutos = 0 puntos","16-30 minutos = 1 puntos","31-60 minutos= 2 puntos",">60 minutos= 3 puntos")
levels(data$pittsburg_pje_q4)=c(">7 horas = 0 puntos","6-7 horas = 1 punto","5-6 horas = 2 puntos","< 5 horas =3 puntos")
levels(data$pittsburg_q5a)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5b)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5c)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5d)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5e)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5f)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5g)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5h)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q5i)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q6)=c("Bastante buena","Buena","Mala","Bastante mala")
levels(data$pittsburg_q7)=c("Ninguna vez en el última mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o mas veces")
levels(data$pittsburg_q8)=c("Ninguna vez en el último mes","Menos de una vez a la semana","Una o dos veces a la semana","Tres o más veces a la semana")
levels(data$pittsburg_q9)=c("Ningún problema","Un problema muy ligero","Algo de problema","Un gran problema")
levels(data$puntaje_latencia_del_sueno)=c("0 = 0 puntos","1-2 = 1 puntos","3-4 = 2 puntos","5-6= 3 puntos")
levels(data$score_componente4)=c(">85% = 0 puntos","75-84% = 1 punto","65-74% = 2 puntos","< 65% = 3 puntos")
levels(data$categorizacion_comp5)=c("0 = 0 puntos","1-9 = 1 punto","10-18= 2 puntos","19-27= 3 puntos")
levels(data$score_disf_nocturna)=c("0 = 0 puntos","1-2= 1 punto","3-4= 2 puntos","5-6= 3 puntos")
levels(data$realizacion_examenes)=c("1","2","3","4","5")
levels(data$exposicion_de_trabajos)=c("1","2","3","4","5")
levels(data$intervencion_clases)=c("1","2","3","4","5")
levels(data$tratar_profesor)=c("1","2","3","4","5")
levels(data$sobrecarga_academica)=c("1","2","3","4","5")
levels(data$falta_de_tiempo)=c("1","2","3","4","5")
levels(data$competitividad_companeros)=c("1","2","3","4","5")
levels(data$realizar_trabajos)=c("1","2","3","4","5")
levels(data$la_tarea_de_estudiar)=c("1","2","3","4","5")
levels(data$problemas_con_profesores)=c("1","2","3","4","5")
levels(data$problemas_companeros)=c("1","2","3","4","5")
levels(data$asistencia_clases)=c("1","2","3","4","5")
levels(data$exceso_de_responsabilidad)=c("1","2","3","4","5")
levels(data$obtener_notas_elevadas)=c("1","2","3","4","5")
levels(data$perspectivas_profesionales)=c("1","2","3","4","5")
levels(data$eleccion_asignaturas)=c("1","2","3","4","5")
levels(data$mantener_beca)=c("1","2","3","4","5")
levels(data$acabar_los_estudios)=c("1","2","3","4","5")
levels(data$presion_familiar)=c("1","2","3","4","5")
levels(data$ansiedadbeck_q1)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q2)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q3)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q4)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q5)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q6)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q7)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q8)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q9)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q10)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q11)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q12)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q13)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q14)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q15)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q16)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q17)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q18)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q19)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q20)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$ansiedadbeck_q21)=c("En absoluto","Levemente","Moderadamente","Severamente")
levels(data$categorizacion_ans_beck)=c("N/A","Muy baja","Moderada","Severa")
levels(data$dep_beck_q1)=c("No me siento triste","Me siento triste gran parte del tiempo","Me siento triste todo el tiempo","Me siento tan triste o soy tan infeliz que no puedo soportarlo")
levels(data$dep_beck_q2)=c("No estoy desalentado respecto a mi futuro","Me siento más desalentado respecto de mi futuro que lo que solía estarlo.","No espero que las cosas funcionen para mi.","Siento que no hay esperanza para mi futuro y que sólo puede empeorar")
levels(data$dep_beck_q3)=c("No me siento como un fracasado.","He fracasado más de lo que hubiera debido.","Cuando miro hacia atrás, veo muchos fracasos.","Siento que como persona soy un fracaso total.")
levels(data$dep_beck_q4)=c("Obtengo tanto placer como siempre por las cosas de las que disfruto.","No disfruto tanto de las cosas como solía hacerlo.","Obtengo muy poco placer de las cosas que solía disfrutar.","No puedo obtener ningún placer de las cosas de las que solía disfrutar.")
levels(data$dep_beck_q5)=c("No me siento particularmente culpable.","Me siento culpable respecto de varias cosas que he hecho o que debería haber hecho.","Me siento bastante culpable la mayor parte del tiempo.","Me siento culpable todo el tiempo.")
levels(data$dep_beck_q6)=c("No siento que este siendo castigado","Siento que tal vez pueda ser castigado","Espero ser castigado","Siento que estoy siendo castigado")
levels(data$dep_beck_q7)=c("Siento acerca de mi lo mismo que siempre.","He perdido la confianza en mí mismo.","Estoy decepcionado conmigo mismo.","No me gusto a mí mismo.")
levels(data$dep_beck_q8)=c("No me critico ni me culpo más de lo habitual.","Estoy más crítico conmigo mismo de lo que solía estarlo.","Me critico a mí mismo por todos mis errores.","Me culpo a mí mismo por todo lo malo que sucede.")
levels(data$dep_beck_q9)=c("No tengo ningún pensamiento de matarme.","He tenido pensamientos de matarme, pero no lo haría.","Querría matarme.","Me mataría si tuviera la oportunidad de hacerlo.")
levels(data$dep_beck_q10)=c("No lloro más de lo que solía hacerlo.","Lloro más de lo que solía hacerlo","Lloro por cualquier pequeñez.","Siento ganas de llorar pero no puedo.")
levels(data$dep_beck_q11)=c("No estoy más inquieto o tenso que lo habitual.","Me siento más inquieto o tenso que lo habitual.","Estoy tan inquieto o agitado que me es difícil quedarme quieto.","Estoy tan inquieto o agitado que tengo que estar siempre en movimiento o haciendo algo.")
levels(data$dep_beck_q12)=c("No he perdido el interés en otras actividades o personas.","Estoy menos interesado que antes en otras personas o cosas.","He perdido casi todo el interés en otras personas o cosas.","Me es difícil interesarme por algo.")
levels(data$dep_beck_q13)=c("Tomo mis propias decisiones tan bien como siempre.","Me resulta más difícil que de costumbre tomar decisiones.","Encuentro mucha más dificultad que antes para tomar decisiones.","Tengo problemas para tomar cualquier decisión.")
levels(data$dep_beck_q14)=c("No siento que yo no sea valioso.","No me considero a mi mismo tan valioso y útil como solía considerarme.","Me siento menos valioso cuando me comparo con otros.","Siento que no valgo nada.")
levels(data$dep_beck_q15)=c("Tengo tanta energía como siempre.","Tengo menos energía que la que solía tener.","No tengo suficiente energía para hacer demasiado.","No tengo energía suficiente para hacer nada.")
levels(data$dep_beck_q16)=c("No he experimentado ningún cambio en mis hábitos de sueño.","Duermo un poco menos que lo habitual /  Duermo un poco más que lo habitual.","Duermo mucho más que lo habitual / Duermo mucho menos que lo habitual.","Duermo la mayor parte del día / Me despierto 1-2 horas más temprano y no puedo volver a dormirme.")
levels(data$dep_beck_q17)=c("No estoy tan irritable que lo habitual.","Estoy más irritable que lo habitual.","Estoy mucho más irritable que lo habitual.","Estoy irritable todo el tiempo.")
levels(data$dep_beck_q18)=c("No he experimentado ningún cambio en mi apetito.","Mi apetito es un poco menor que lo habitual / Mi apetito es un poco mayor que lo habitual.","Mi apetito es mucho menor que antes / Mi apetito es mucho mayor que lo habitual.","No tengo apetito en absoluto / Quiero comer todo el día.")
levels(data$dep_beck_q19)=c("Puedo concentrarme tan bien como siempre.","No puedo concentrarme tan bien como habitualmente.","Me es difícil mantener la mente en algo por mucho tiempo.","Encuentro que no puedo concentrarme en nada.")
levels(data$dep_beck_q20)=c("No estoy más cansado o fatigado que lo habitual.","Me fatigo o me canso más fácilmente que lo habitual.","Estoy demasiado fatigado o cansado para hacer muchas de las cosas que solía hacer.","Estoy demasiado fatigado o cansado para hacer la mayoría de las cosas que solía hacer.")
levels(data$dep_beck_q21)=c("No he notado ningún cambio reciente en mi interés por el sexo.","Estoy menos interesado en el sexo de lo que solía estarlo.","Estoy mucho menos interesado en el sexo.","He perdido completamente el interés en el sexo.")
levels(data$categorizacion_dep_beck)=c("Mínima depresión","Depresión leve","Depresión moderada","Depresión grave")

chronic <- copy(data)
rm(data)

save(chronic, file = "data/chronic.RData")
