

# Module ersip_iolist #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>join list of iolists with separator.</td></tr><tr><td valign="top"><a href="#trim_head_lws-1">trim_head_lws/1</a></td><td>trim leading linear whitesapaces (SP or HTABs).</td></tr><tr><td valign="top"><a href="#trim_lws-1">trim_lws/1</a></td><td></td></tr><tr><td valign="top"><a href="#trim_tail_lws-1">trim_tail_lws/1</a></td><td>trim trailing linear whitesapaces (SP or HTABs).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_empty-1"></a>

### is_empty/1 ###

<pre><code>
is_empty(X::iolist()) -&gt; boolean()
</code></pre>
<br />

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(Sep, List) -&gt; iolist()
</code></pre>

<ul class="definitions"><li><code>List = [iolist() | binary()]</code></li><li><code>Sep = iolist() | binary() | char()</code></li></ul>

join list of iolists with separator

<a name="trim_head_lws-1"></a>

### trim_head_lws/1 ###

<pre><code>
trim_head_lws(Rest::iolist()) -&gt; iolist()
</code></pre>
<br />

trim leading linear whitesapaces (SP or HTABs).

<a name="trim_lws-1"></a>

### trim_lws/1 ###

<pre><code>
trim_lws(L::iolist()) -&gt; iolist()
</code></pre>
<br />

<a name="trim_tail_lws-1"></a>

### trim_tail_lws/1 ###

<pre><code>
trim_tail_lws(L::iolist()) -&gt; iolist()
</code></pre>
<br />

trim trailing linear whitesapaces (SP or HTABs).

