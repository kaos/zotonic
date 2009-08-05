{% extends "base.tpl" %}

{% block title %}Zotonic &ndash; Pragmatic innovation. Stuff that works{% endblock %}

{% block content %}
	<h1>Welcome to Zotonic.com</h1>
	<p>					
		This will grow into the website about the Erlang CMS Zotonic.
		At this moment Zotonic is still in development. This website will be up somewhere in the next couple of weeks.
	</p>

	<h2>Zotonic is build on open-source components</h2>
	<p>Zotonic is based upon some great open-source software. We stand for open and love the communities behind these packages.</p>

	<h3>WebMachine</h3>
	<p><a href="http://code.google.com/p/webmachine/" title="WebMachine" rel="external">WebMachine</a> is an abstraction of the 
		HTTP protocol. It enables us to easily define resources without thinking of the correct HTTP status codes and protocol flow.</p>

	<h3>MochiWeb</h3>
	<p><a href="http://code.google.com/p/mochiweb/" title="MochiWeb" rel="external">MochiWeb</a> is a toolkit for building 
		high performance web servers. WebMachine is built directly on top of MochiWeb.</p>

	<h3>Nitrogen</h3>
	<p><a href="http://www.nitrogenproject.com" title="Nitrogen" rel="external">Nitrogen</a> brings cutting-edge web development 
		to Erlang. We studied some parts of Nitrogen:</p>	
	<ul class="regular">
		<li>The communication with the user-agent is directly inspired by (and partly copied from) Nitrogen.</li>
		<li>Actions, validators and events, though quite changed in Zotonic.</li>
		<li>Utility code.</li>
	</ul>

	<h3>ErlyDTL</h3>
	<p><a href="http://code.google.com/p/erlydtl/" title="ErlyDTL" rel="external">ErlyDTL</a> is an Erlang implemenation of the Django Template Language. It implements a subset of DTL. We have added a couple of extras to ErlyDTL:</p>
	
	<ul class="regular">
		<li>Scomps - cacheable screen components, implemented by Erlang modules.</li>
		<li>Automatic id generation with #name. With automatic ids you can easily use unique ids in templates that are 
			included more than once. This is done by prefixing the name with an unique string. For example {{ #name }}
			might generate fdhgw-auto-name.</li>
		<li>Url generation. We implemented the {% url name par=value par=value %} extension. This will lookup the name 
			in the dispatch rules and generate the best url matching the name and the parameters.</li>
	</ul>
{% endblock %}


{% block sidebar %}
	<h1>Erlang powered</h1>
	<p>
		Zotonic is Erlang powered. Erlang is a programming language which has many features more commonly associated with an 
		operating system than with a programming language: concurrent processes, scheduling, memory management, distribution, 
		networking and more.
	</p>
{% endblock %}
