<img name="footer" src="/lib/images/footer.gif" width="957" height="77" border="0" usemap="#m_footer" alt="">

<map name="m_footer">
	<area shape="rect" coords="782,20,957,67" href="http://www.iamsterdam.com/" target="_blank" title="I Amsterdam" alt="I Amsterdam" >
	<area shape="rect" coords="676,0,767,77" href="http://www.flandershouse.org/" target="_blank" title="Flanders Suits You" alt="Flanders Suits You" >
	<area shape="rect" coords="519,16,648,72" href="http://www.fryslan.nl/sjablonen/1/infotype/webpage/view.asp?objectID=1025" target="_blank" title="Province of Fryslan" alt="Province of Fryslan">
	<area shape="poly" coords="348,15,483,15,483,72,348,72,348,15" href="http://www.ing.com/group/index.jsp?&lang=en" target="_blank" title="ING" alt="ING" >
	<area shape="poly" coords="229,12,308,12,308,72,229,72,229,12" href="http://newyork.timeout.com/" target="_blank" title="NY Time Out" alt="NY Time Out" >
	<area shape="rect" coords="128,12,194,76" href="http://amstellight.com/" target="_blank" title="Amstel Light" alt="Amstel Light" >
	<area shape="poly" coords="6,11,108,11,108,72,6,72,6,11" href="http://www.heineken.com/" target="_blank" title="heineken" alt="heineken" >
</map>

{% if m.rsc.footer_collection.summary %}
<p class="clear footer-text">
	{{ m.rsc.footer_collection.summary }} <a href="http://www.mannschaft.org" title="Website by Mannschaft">Mannschaft</a> &mdash; <a href="http://www.zotonic.com" title="Powered by Zotonic">Zotonic</a>
	<span>
		<a href="http://www.timbenniks.nl">Tim Benniks</a>
		<a href="http://www.whatwebwhat.com">Marc Worrell</a>
	</span>
</p>
{% endif %}