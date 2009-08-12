<a href="{{ m.rsc.ny400.page_url }}" title="{{ m.rsc.ny400.title }}">{% image m.rsc.ny400_logo.media[1] %}</a>

{% cache 3600 news cat="news" %}

<h1>Latest news</h1>

<ul class="items-list">
	{% for id in m.search[{latest cat="news" pagelen="5"}] %}
		{% include "_news_item.tpl" id=id %}
	{% empty %}
	<li>
		No news to show.
	</li>
	{% endfor %}
	
	<p><a href="/news">All news.</a></p>
	
</ul>
{% endcache %}
