<a href="http://www.ny400.org/" title="{{ m.rsc.ny400.title }}">{% image m.rsc.ny400_logo.media[1] %}</a>

{% cache 3600 news cat="news" %}

<h1 class="float: left; display: block;">Latest news<a href="/feed/news" title="news feed"><img src="/lib/images/feed.gif" alt="" width="20" style="margin: 0 0 0 3px;" /></a></h1>

<ul class="items-list">
	{% for id in m.search[{latest cat="news" pagelen="5"}] %}
		{% include "_news_item.tpl" id=id %}
	{% empty %}
	<li>
		No news to show.
	</li>
	{% endfor %}
	
	<p><a href="{% url news_overview %}">All news.</a></p>
	
</ul>
{% endcache %}
