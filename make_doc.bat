cd memoria
call make
cd ..


rem Copiar PDFs

copy memoria\memoria.pdf dvd\memoria.pdf
copy memoria\memoria.pdf dvd\documentacion\memoria.pdf
copy memoria\extras\gestor_recursos*.pdf dvd\documentacion\GestorRecursos\
copy memoria\extras\dibujador*.pdf dvd\documentacion\MotorGrafico\
copy memoria\extras\motor_opengl*.pdf dvd\documentacion\MotorGrafico\
copy memoria\extras\objeto*.pdf dvd\documentacion\MotorGrafico\
copy memoria\extras\motor_procesos*.pdf dvd\documentacion\MotorProcesos\
copy memoria\extras\planetario*.pdf dvd\documentacion\Planetario\
