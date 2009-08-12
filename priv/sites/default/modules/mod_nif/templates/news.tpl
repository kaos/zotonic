{% extends "base.tpl" %}

{% block title %}
	{{ m.rsc.news.title }}
{% endblock %}

{% block pageclass %}
	{{ m.rsc.news.slug }}
{% endblock %}

{% block pageheader %}
	<h1 id="header">New Island Festival. Created by Dutch artists. Governors Island september 10-20</h1>
{% endblock %}

{% block content %}
	
	<div id="content-wrapper" class="clearfix">
		<div id="content">
			<div class="padding">
				{% include "_view.tpl" %}
	
				<ul class="news-list clearfix">
					{% for id in m.search[{latest cat="news"}] %}
						{% include "_news_item.tpl" id=id %}
					</li>
					{% empty %}
					<li>
						No news to show.
					</li>
					{% endfor %}
				</ul>
							
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}