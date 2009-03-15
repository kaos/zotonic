{% extends "base.tpl" %}

{# comment #}

{% block title %}Search{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2 class="header-alone">Zoek resultaat</h2>
		
		<h3 class="block">Trek urban</h3>
		<div class="block clearfix">
			<p>
				{% image "trek_urban.jpg" width=120 height=130 crop alt="Handgrepen" %}
				Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
				Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
			</p>
		</div>
		
		<h3 class="block">Bike</h3>
		<div class="block clearfix">
			<p>
				{% image "trek_urban.jpg" width=120 height=130 crop alt="Handgrepen" %}
				Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
			</p>
		</div>
		
		<h3 class="block">About Hans Struijk</h3>
		<div class="block clearfix">
			<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
		</div>
		
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			{% include "_subnav.tpl" %}
		</div>
	</div>
{% endblock %}