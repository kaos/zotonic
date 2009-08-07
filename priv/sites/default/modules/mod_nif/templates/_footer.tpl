<ul id="sponsor-logos" class="clearfix">
	{% for id in m.rsc.footer_collection.o.collection_member %}
	<li class="clearfix">
		{% if m.rsc[id].media %}
			<div class="item-image left">
				<a href="{{ m.rsc[id].website }}" title="{{ m.rsc[id].title }}" target="_blank">{% image m.rsc[id].media[1] height=40 %}</a>
			</div>
		{% endif %}
	</li>
	{% endfor %}
</ul>

{% if m.rsc.footer_collection.summary %}
<p class="footer-text">
	{{ m.rsc.footer_collection.summary }} <a href="http://www.mannschaft.org" title="Website by Mannschaft">Mannschaft</a> &mdash; <a href="http://www.zotonic.com" title="Powered by Zotonic">Zotonic
	<span><a href="http://www.timbenniks.nl">Tim Benniks</a>, <a href="http://www.whatwebwhat.com">Marc Worrell</a></span>
</p>
{% endif %}