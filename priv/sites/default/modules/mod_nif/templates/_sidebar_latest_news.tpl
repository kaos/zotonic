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
</ul>
{% endcache %}
