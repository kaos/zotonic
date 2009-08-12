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

{% block content %}
	
	<div id="content-wrapper" class="clearfix">
		<div id="content" class="zp-65">
			<div class="padding">
				{% include "_view.tpl" %}
				
				{% cache 3600 artists cat="artist" %}
				{% with m.search[{all_bytitle cat="artist" pagelen=200}] as result %}

					<ul class="artists-list clearfix">

						{% for title, id in result %}
							<li class="clearfix">
								<h2><a href="{{ m.rsc[id].page_url }}" title="{{ title }}">{{ title }}</a></h2>
								{% if m.rsc[id].media[1] %}
								<div class="item-image left">
									<a href="{{ m.rsc[id].page_url }}" title="{{ title }}">
										{% image m.rsc[id].media[1] width=180 height=90 crop %}
									</a>
								</div>
								{% endif %}
							
								{% if m.rsc[id].summary %}
									<p>{{ m.rsc[id].summary }}</p>
								{% endif %}
							</li>
						{% empty %}
							<li>
								<p>No artists have been found.</p>
							</li>
						{% endfor %}
				
					</ul>
					
				{% endwith %}
				{% endcache %}
			
			</div>
		</div>
		
		<div id="sidebar" class="zp-30">
			<div class="padding">
				{% include "_sidebar_latest_news.tpl" %}
			</div>
			<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
		</div>
	</div>
{% endblock %}