#!/bin/bash

epmd -daemon
/erlang-ansible-rpc/erlang_rpc $@
