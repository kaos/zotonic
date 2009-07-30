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
		<div id="content">
			<div class="padding">
				{% include "_view.tpl" %}
				
				<ul class="collection-list clearfix">
					{% for id in m.rsc[id].o.collection_member %}
					<li class="clearfix">
						
						{% if m.rsc[id].media %}
							<div class="item-image left">{% image m.rsc[id].media[1] height=150 %}</div>
						{% endif %}
					</li>
					{% empty %}
					<li>
						No links to show.
					</li>
					{% endfor %}
				</ul>
				
			</div>
		</div>
	</div>
{% endblock %}