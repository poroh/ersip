

# Module ersip_branch #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-branch">branch()</a> ###


<pre><code>
branch() = {branch, binary()}
</code></pre>




### <a name="type-branch_key">branch_key()</a> ###


<pre><code>
branch_key() = {branch_key, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_rfc3261-1">is_rfc3261/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Create comparable key for branch parameter.</td></tr><tr><td valign="top"><a href="#make_random-1">make_random/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_rfc3261-1">make_rfc3261/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-branch">branch()</a>) -&gt; binary()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(X1::<a href="#type-branch">branch()</a>) -&gt; binary()
</code></pre>
<br />

<a name="is_rfc3261-1"></a>

### is_rfc3261/1 ###

<pre><code>
is_rfc3261(X1::<a href="#type-branch">branch()</a> | <a href="#type-branch_key">branch_key()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-branch">branch()</a>
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(Key::<a href="#type-branch">branch()</a>) -&gt; <a href="#type-branch_key">branch_key()</a>
</code></pre>
<br />

Create comparable key for branch parameter.

<a name="make_random-1"></a>

### make_random/1 ###

<pre><code>
make_random(NumBytes::pos_integer()) -&gt; <a href="#type-branch">branch()</a>
</code></pre>
<br />

<a name="make_rfc3261-1"></a>

### make_rfc3261/1 ###

<pre><code>
make_rfc3261(Bin::binary()) -&gt; <a href="#type-branch">branch()</a>
</code></pre>
<br />

