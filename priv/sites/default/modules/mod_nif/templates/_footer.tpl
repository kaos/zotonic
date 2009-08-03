<ul id="sponsor-logos">
	{% for id in m.rsc.footer_collection.o.collection_member %}
	<li class="clearfix">
		{% if m.rsc[id].media %}
			<div class="item-image left">
				<a href="{{ m.rsc[id].website }}" title="{{ m.rsc[id].title }}" target="_blank">{% image m.rsc[id].media[1] height=30 %}</a>
			</div>
		{% endif %}
	</li>
	{% endfor %}
</ul>