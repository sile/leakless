

# Module leakless_table #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


エントリへのリンクや監視が行えるKey-Valueテーブル.
Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>


__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-connection">connection()</a> ###



<pre><code>
connection() = reference()
</code></pre>





### <a name="type-disconnect_info">disconnect_info()</a> ###



<pre><code>
disconnect_info() = #{table =&gt; {leakless_table, pid()}, edge_spec =&gt; <a href="#type-edge_spec">edge_spec()</a>, connection =&gt; <a href="#type-connection">connection()</a>, reason =&gt; <a href="#type-disconnect_reason">disconnect_reason()</a>}
</code></pre>





### <a name="type-disconnect_reason">disconnect_reason()</a> ###



<pre><code>
disconnect_reason() = {'EXIT', <a href="#type-peer">peer()</a>, Reason::term()} | {erased, Eraser::pid()} | noentry | {table_down, Reason::term()}
</code></pre>





### <a name="type-down_message">down_message()</a> ###



<pre><code>
down_message() = {'DOWN', <a href="#type-connection">connection()</a>, leakless_table_entry, <a href="#type-key">key()</a>, {'DISCONNECTED', <a href="#type-disconnect_info">disconnect_info()</a>}}
</code></pre>





### <a name="type-edge_spec">edge_spec()</a> ###



<pre><code>
edge_spec() = {<a href="#type-edge_type">edge_type()</a>, <a href="#type-peer">peer()</a>, <a href="#type-key">key()</a>}
</code></pre>





### <a name="type-edge_type">edge_type()</a> ###



<pre><code>
edge_type() = link | forward_link | backward_link | monitor
</code></pre>





### <a name="type-exit_message">exit_message()</a> ###



<pre><code>
exit_message() = {'EXIT', pid(), {'DISCONNECTED', <a href="#type-disconnect_info">disconnect_info()</a>}}
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = term()
</code></pre>





### <a name="type-peer">peer()</a> ###



<pre><code>
peer() = pid()
</code></pre>





### <a name="type-start_option">start_option()</a> ###



<pre><code>
start_option() = link | {name, <a href="leakless.md#type-otp_proc_name">leakless:otp_proc_name()</a>}
</code></pre>





### <a name="type-start_options">start_options()</a> ###



<pre><code>
start_options() = [<a href="#type-start_option">start_option()</a>]
</code></pre>





### <a name="type-table_ref">table_ref()</a> ###



<pre><code>
table_ref() = <a href="leakless.md#type-otp_proc_ref">leakless:otp_proc_ref()</a>
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#disconnect-2">disconnect/2</a></td><td></td></tr><tr><td valign="top"><a href="#disconnect_all-3">disconnect_all/3</a></td><td></td></tr><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#flush-1">flush/1</a></td><td></td></tr><tr><td valign="top"><a href="#flush-3">flush/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_opt-1">start_opt/1</a></td><td></td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td></td></tr><tr><td valign="top"><a href="#store_connect-5">store_connect/5</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-4"></a>

### connect/4 ###


<pre><code>
connect(Table::<a href="#type-table_ref">table_ref()</a>, Type::<a href="#type-edge_type">edge_type()</a>, Peer::<a href="#type-peer">peer()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-connection">connection()</a>
</code></pre>
<br />


<a name="disconnect-2"></a>

### disconnect/2 ###


<pre><code>
disconnect(Table::<a href="#type-table_ref">table_ref()</a>, Connection::<a href="#type-connection">connection()</a>) -&gt; ok
</code></pre>
<br />


<a name="disconnect_all-3"></a>

### disconnect_all/3 ###


<pre><code>
disconnect_all(Table::<a href="#type-table_ref">table_ref()</a>, Peer::<a href="#type-peer">peer()</a>, Key::<a href="#type-key">key()</a>) -&gt; [<a href="#type-connection">connection()</a>]
</code></pre>
<br />


<a name="erase-2"></a>

### erase/2 ###


<pre><code>
erase(Table::<a href="#type-table_ref">table_ref()</a>, Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />


<a name="find-2"></a>

### find/2 ###


<pre><code>
find(Table::<a href="#type-table_ref">table_ref()</a>, Key::<a href="#type-key">key()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | error
</code></pre>
<br />


<a name="flush-1"></a>

### flush/1 ###


<pre><code>
flush(Connection::<a href="#type-connection">connection()</a>) -&gt; ok
</code></pre>
<br />


<a name="flush-3"></a>

### flush/3 ###


<pre><code>
flush(Table::<a href="#type-table_ref">table_ref()</a>, Peer::<a href="#type-peer">peer()</a>, Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />


<a name="start_opt-1"></a>

### start_opt/1 ###


<pre><code>
start_opt(Options::<a href="#type-start_options">start_options()</a>) -&gt; {ok, pid()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = {already_started, pid()} | term()</code></li></ul>


<a name="store-3"></a>

### store/3 ###


<pre><code>
store(Table::<a href="#type-table_ref">table_ref()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = {already_exists, <a href="#type-value">value()</a>}</code></li></ul>


<a name="store_connect-5"></a>

### store_connect/5 ###


<pre><code>
store_connect(Table::<a href="#type-table_ref">table_ref()</a>, Type::<a href="#type-edge_type">edge_type()</a>, Peer::<a href="#type-peer">peer()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; {ok, <a href="#type-connection">connection()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = {already_exists, <a href="#type-value">value()</a>}</code></li></ul>


<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(Table::<a href="#type-table_ref">table_ref()</a>) -&gt; [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]
</code></pre>
<br />


