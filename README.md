Erlang RPC for ansible
=====

If you need an Erlang RPC call in an ansible playbook (on the host or via
Docker container)

Include path to ``erlang_rpc`` and/or ``erlang_rpc_docker`` in either:
- ``ANSIBLE_LIBRARY`` env variable
- ``--module-path`` command line option
- ``library`` config entry (see examples)
- simply or copy them to ``your-playbooks/library`` directory

Directly on host
------------
Define the task as follows:
```[yml]
- erlang_rpc:
    node: test@localhost
    nametype: longnames
    cookie: secret_cookie
    module: erlang
    function: system_info
    args: "[otp_release]"
```

Via Docker container
--------------
Useful when you don't have Erlang on your host machine or want to attach
to one of the Docker overlay networks.

```[yml]
- erlang_rpc_docker:
    node: test@10.0.0.5
    nametype: longnames
    cookie: secret_cookie
    module: erlang
    function: system_info
    args: "[otp_release]" # defaults to "[]"
    network: my-overlay-network  # defaults to "bridge"
```

Examples
--------
See ``examples`` directory for an example of either direct or Docker call.
Use ``example.yml`` playbook to try it locally.

You need Erlang (for both) and Docker (for the Docker example).
