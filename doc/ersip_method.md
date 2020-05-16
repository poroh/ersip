

# Module ersip_method #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-method">method()</a> ###


<pre><code>
method() = {method, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ack-0">ack/0</a></td><td></td></tr><tr><td valign="top"><a href="#bye-0">bye/0</a></td><td></td></tr><tr><td valign="top"><a href="#cancel-0">cancel/0</a></td><td></td></tr><tr><td valign="top"><a href="#invite-0">invite/0</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#notify-0">notify/0</a></td><td></td></tr><tr><td valign="top"><a href="#options-0">options/0</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#prack-0">prack/0</a></td><td></td></tr><tr><td valign="top"><a href="#refer-0">refer/0</a></td><td></td></tr><tr><td valign="top"><a href="#register-0">register/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-0">subscribe/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ack-0"></a>

### ack/0 ###

`ack() -> any()`

<a name="bye-0"></a>

### bye/0 ###

`bye() -> any()`

<a name="cancel-0"></a>

### cancel/0 ###

`cancel() -> any()`

<a name="invite-0"></a>

### invite/0 ###

`invite() -> any()`

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-method">method()</a>
</code></pre>
<br />

<a name="notify-0"></a>

### notify/0 ###

`notify() -> any()`

<a name="options-0"></a>

### options/0 ###

`options() -> any()`

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-method">method()</a>)
</code></pre>
<br />

<a name="prack-0"></a>

### prack/0 ###

`prack() -> any()`

<a name="refer-0"></a>

### refer/0 ###

`refer() -> any()`

<a name="register-0"></a>

### register/0 ###

`register() -> any()`

<a name="subscribe-0"></a>

### subscribe/0 ###

`subscribe() -> any()`

<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(X1::<a href="#type-method">method()</a>) -&gt; binary()
</code></pre>
<br />

