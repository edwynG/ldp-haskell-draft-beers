IFACEDIR = iface
SRCDIR = src
OBJDIR = obj
BINDIR = bin

FILE = draftbeers
ENTRYPOINT = main.hs

all: prepare $(BINDIR)/$(FILE)

clean: # Regla para limpiar los archivos generados
	@echo "Limpiando archivos generados..."
	rm -rf $(OBJDIR) $(BINDIR) $(IFACEDIR)

prepare: # Regla para preparar los directorios
	mkdir -p $(OBJDIR) $(BINDIR) $(IFACEDIR)

$(BINDIR)/$(FILE): prepare	# Regla para compilar el programa
	@echo "Compilando el programa..."
	cd $(SRCDIR) && ghc $(ENTRYPOINT) -odir ../$(OBJDIR) -hidir ../$(IFACEDIR) -o ../$(BINDIR)/$(FILE)

execute: $(BINDIR)/$(FILE) # Regla para ejecutar el programa
	@echo "Ejecutando el programa..."
	./$(BINDIR)/$(FILE)
	@echo ""
	@echo "Fin de ejecucion."