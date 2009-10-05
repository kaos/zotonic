{% extends "base.tpl" %}

{% block title %}Stuff that works{% endblock %}

{% block page_class %}home{% endblock %}

{% block banner %}

	<section id="banner">
		<section id="download-zotonic">
			<a href="#" title=""><img src="./lib/images/download_zotonic.png" alt="" /></a>
		</section>
		<section id="docs-zotonic">
			<a href="#" title=""><img src="./lib/images/docs.png" alt="" /></a>
		</section>
	</section>

{% endblock %}

{% block banner %}

	<section id="banner">
		<section id="download-zotonic">
			<a href="#" title=""><img src="./lib/images/download_zotonic.png" alt="" /></a>
		</section>
		<section id="docs-zotonic">
			<a href="#" title=""><img src="./lib/images/docs.png" alt="" /></a>
		</section>
	</section>

{% endblock %}

{% block content %}

	<article id="content" class="zp-67">
		<div class="padding">
			{% with m.rsc.home as r %}
				<h1>ssss{{ r.title }}</h1>

				<p class="summary">
					{{ r.summary }}
				</p>

				{{ r.body }}
			{% endwith %}
		</div>
	</article>

{% endblock %}

{% block sidebar %}
	
	<aside id="sidebar" class="zp-33">
		<h1>Zotonic is Open Source</h1>
		<p>Zotonic is released under the Open Source <a href="#">Apache2 license</a>, which gives you the possibility to use it and modify it in every circumstance.</p>
		<p><a href="#">Read more about Zotonic &raquo;</a></p>
	</aside>

{% endblock %}