

# Module leakless #
* [Description](#description)
* [Data Types](#types)


共通的に使われる関数や型を定義しているモジュール.
Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>


<a name="types"></a>

## Data Types ##




### <a name="type-otp_proc_name">otp_proc_name()</a> ###



<pre><code>
otp_proc_name() = {local, Name::atom()} | {global, Name::term()} | {via, module(), Name::term()}
</code></pre>



[`gen_server:start/4`](gen_server.md#start-4)等で指定可能な起動プロセスの名前



### <a name="type-otp_proc_ref">otp_proc_ref()</a> ###



<pre><code>
otp_proc_ref() = (Name::atom()) | {Name::atom(), node()} | {global, Name::term()} | {via, module(), Name::term()} | pid()
</code></pre>



[`gen_server:cast/2`](gen_server.md#cast-2)等で指定可能な宛先プロセスの参照
