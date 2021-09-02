all:
#	service
	rm -rf ebin/* *_ebin test_10 lgh *.lgh varmdo;
	erlc -I ../interfaces -o ebin src/*.erl;
	rm -rf src/*.beam *.beam  test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
	echo Done
test_10_unit_test:
	rm -rf test_10_ebin;
	rm -rf src/*.beam *.beam test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	mkdir test_10_ebin;
#	interface
	erlc -I ../interfaces -o test_10_ebin ../interfaces/*.erl;
#	support
	cp ../applications/support/src/*.app test_10_ebin;
	erlc -I ../interfaces -o test_10_ebin ../kube_support/src/*.erl;
	erlc -I ../interfaces -o test_10_ebin ../applications/support/src/*.erl;
#	etcd
	cp ../applications/etcd/src/*.app test_10_ebin;
	erlc -I ../interfaces -o test_10_ebin ../kube_dbase/src/*.erl;
	erlc -I ../interfaces -o test_10_ebin ../applications/etcd/src/*.erl;
#	kubelet
	cp ../applications/kubelet/src/*.app test_10_ebin;
	erlc -I ../interfaces -o test_10_ebin ../node/src/*.erl;
	erlc -I ../interfaces -o test_10_ebin ../applications/kubelet/src/*.erl;
#	iaas
	erlc -I ../interfaces -o test_10_ebin src/*.erl;
#	test application
	mkdir test_ebin;
	cp test_src/*.app test_ebin;
	erlc -I ../interfaces -o test_ebin test_src/*.erl;
	erl -pa test_10_ebin -pa test_ebin\
	    -setcookie test_10_cookie\
	    -sname iaas_test_10\
	    -unit_test monitor_node iaas_test_10\
	    -unit_test cluster_id test_10\
	    -unit_test start_host_id c1_varmdo\
	    -run unit_test start_test test_src/test.config
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
#	etcd
	cp ../applications/etcd/src/*.app varmdo_ebin;
	erlc -I ../interfaces -o varmdo_ebin ../kube_dbase/src/*.erl;
	erlc -I ../interfaces -o varmdo_ebin ../applications/etcd/src/*.erl;
#	kubelet
	cp ../applications/kubelet/src/*.app varmdo_ebin;
	erlc -I ../interfaces -o varmdo_ebin ../node/src/*.erl;
	erlc -I ../interfaces -o varmdo_ebin ../applications/kubelet/src/*.erl;
#	cluster
	cp ../applications/cluster/src/*.app varmdo_ebin;
	erlc -I ../interfaces -o varmdo_ebin ../applications/cluster/src/*.erl;
	erlc -I ../interfaces -o varmdo_ebin src/*.erl;
#	kube_pod
	erlc -I ../interfaces -o varmdo_ebin ../kube_pod/src/*.erl;
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
