{% extends "base.tpl" %}

{% block title %}
	{{ m.rsc[id].title }}
{% endblock %}

{% block pageclass %}
	{{ m.rsc[id].slug }}
{% endblock %}

{% block pageheader %}
	<h1 id="header">New Island Festival. Created by Dutch artists. Governors Island september 10-20</h1>
{% endblock %}	

{% block navigation %}
	<ul id="navigation" class="list clearfix at-menu">
		<li><a href="/page.html" class="current">About</a></li>

		<li><a href="/page.html">Program</a></li>
		<li><a href="/page.html">Artists</a></li>
		<li>
			<a href="/page.html">Info &amp; Tickets</a>
			<ul>
				<li><a href="#">Information</a></li>

				<li><a href="#">Tickets</a></li>
				<li><a href="#">Food</a></li>
			</ul>
		</li>
		<li><a href="/page.html">Press</a></li>
		<li><a href="/page.html">Links</a></li>
	</ul>
{% endblock %}

{% block content %}
	<div id="content-wrapper" class="clearfix">
		<div id="content" class="zp-65">
			<div class="padding">
				
				<h1>{{ m.rsc[id].title }}</h1>

				{% if m.rsc[id].summary %}
					<p class="intro">{{ m.rsc[id].summary }}</p>
				{% endif %}

				{% if m.rsc[id].body %}
					{{ m.rsc[id].body }}
				{% endif %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				<h1>Latest news items</h1>
				<ul class="items-list">
					<li class="clearfix">
						<h3>News item title</h3>
						<p class="intro">
							<em>August 9 2009</em> &mdash; 
							Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqu&#0133;
							<a href="#">Read&nbsp;more</a>
						</p>
					</li>
					<li class="clearfix">
						<h3>A new newsitem title for the news</h3>
						<div class="item-image left"><img src="./lib/images/koe.jpg" width="65" height="65" alt="" /></div>
						<p class="intro">
							<em>August 9 2009</em> &mdash; 
							Lorem ipsum dolor sit amet, consectetur sed do eiusmod.
							<a href="#">Read&nbsp;more</a>
						</p>
					</li>
					<li class="clearfix">
						<h3>News item title and this title hopefully wraps to two lines</h3>
						<p class="intro">
							<em>September 24 2009</em> &mdash; 
							Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor&#0133;
							<a href="#">Read&nbsp;more</a>
						</p>
					</li>
				</ul>
			</div>
		</div>
	</div>
{% endblock %}
