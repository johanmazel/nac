
default:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build -use-ocamlfind

clean:
	./clean_oasis.sh
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -clean
	ocaml setup.ml -distclean

oasis:
	ocaml setup.ml -build

merlin:
	#create Merlin file
	find `echo "src" | xargs -n 1 -d ' ' readlink -f` \( -name .svn -name tests -o -name doc -o -name result -o -name -o -name oracle -o -name "*.cache" \) -prune -o \( -type d -printf "S %p\n"  \) > .merlin
	find `echo "_build" | xargs -n 1 -d ' ' readlink -f` \( -name .svn -name tests -o -name doc -o -name result -o -name -o -name oracle -o -name "*.cache" \) -prune -o \( -type d -printf "B %p\n"  \) >> .merlin
	echo "PKG async" >> .merlin
	echo "PKG batteries" >> .merlin
	echo "PKG bigarray" >> .merlin
	echo "PKG bin_prot" >> .merlin
	echo "PKG bitstring" >> .merlin
	echo "PKG config-file" >> .merlin
	echo "PKG core" >> .merlin
	echo "PKG core_extended" >> .merlin
	echo "PKG csv" >> .merlin
	echo "PKG gsl" >> .merlin
	echo "PKG ipaddr" >> .merlin
	echo "PKG ocamlgraph" >> .merlin
	echo "PKG parmap" >> .merlin
	echo "PKG uint" >> .merlin
	echo "PKG uint32" >> .merlin
	echo "PKG uint64" >> .merlin
	echo "PKG uint128" >> .merlin
	echo "PKG uri" >> .merlin
	echo "PKG uri.services" >> .merlin
	echo "PKG xml-light" >> .merlin
	echo "PKG xmlm" >> .merlin
	echo "PKG csv" >> .merlin
	echo "PKG yojson" >> .merlin
	echo "PKG functory" >> .merlin
	echo "PKG itv-tree" >> .merlin
	echo "PKG cstruct" >> .merlin
	echo "PKG admd" >> .merlin
	echo "PKG pcap-format" >> .merlin

	echo "PKG robinet_parsing" >> .merlin
	echo "PKG jl" >> .merlin
	echo "PKG netralys_packet_data" >> .merlin
	echo "PKG netralys_flow" >> .merlin
	echo "PKG netralys_metrics" >> .merlin
	echo "PKG netralys_attribute_value" >> .merlin
	echo "PKG netralys_ipaddr_container" >> .merlin
	echo "PKG nac_lib" >> .merlin
	echo "PKG nac_taxonomy" >> .merlin




