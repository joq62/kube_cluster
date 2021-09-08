all:
#	service
	rm -rf ebin/* *_ebin test_10 lgh *.lgh varmdo;
	erlc -I ../interfaces -o ebin src/*.erl;
	rm -rf src/*.beam *.beam  test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
	echo Done
unit_test:
	rm -rf varmdo_ebin deployment varmdo;
	rm -rf src/*.beam *.beam test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	mkdir varmdo_ebin;
	mkdir test_ebin;
#	interface
	erlc -I ../interfaces -o varmdo_ebin ../interfaces/*.erl;
#	support
	cp ../applications/support/src/*.app varmdo_ebin;
	erlc -I ../interfaces -o varmdo_ebin ../kube_support/src/*.erl;
	erlc -I ../interfaces -o varmdo_ebin ../applications/support/src/*.erl;
#	cluster
	cp ../applications/cluster/src/*.app varmdo_ebin;
	erlc -I ../interfaces -o varmdo_ebin ../applications/cluster/src/*.erl;
	erlc -I ../interfaces -o varmdo_ebin src/*.erl;
#	test application
	cp test_src/*.app test_ebin;
	erlc -I ../interfaces -o test_ebin test_src/*.erl;
	erl -pa varmdo_ebin -pa test_ebin\
	    -setcookie varmdo_cookie\
	    -sname cluster_varmdo\
	    -unit_test monitor_node cluster_varmdo\
	    -unit_test cluster_id varmdo\
	    -unit_test cookie varmdo_cookie\
	    -run unit_test start_test test_src/test.config
