{# Show all details of an order #}

<style>
	table td, table th {
		padding: 4px;
	}
</style>

<table width="100%">
	<tr>
		<td colspan="9">
			<h4>Klantgegevens</h4>
			
			{{order.first_name}} {{order.lastname_prefix}} {{ order.lastname}}<br/>
			{{order.email}}<br/>
			{{order.phone}}<br/>
			<br/>
		</td>
	</tr>
	
	<tr>
		<td colspan="4">
			<h4>Factuur adres</h4>

			{{ order.attn }}<br/>
			{{ order.street }}<br/>
			{{ order.postcode }} {{order.city}}<br/>
			{{ order.country }}<br/>
			<br/>
		</td>

		<td colspan="5">
			<h4>Aflever adres</h4>
			{{ order.delivery_attn }}<br/>
			{{ order.delivery_street }}<br/>
			{{ order.delivery_postcode }} {{order.delivery_city}}<br/>
			{{ order.delivery_country }}
		</td>
	</tr>
	
	<tr>
		<td colspan="9">
			<h4>Bestelling</h4>
			
			Nummer: {{ order.id }}<br/>
			<a href="/order/view/{{ order.name }}">Bestellings pagina op Internet &raquo;</a>
		</td>
	</tr>
	
	<tr>
		<th align="left">Artikel Nr</th>
		<th align="left">Omschrijving</th>
		<th align="left">Variatie</th>
		<th align="right">Excl.</th>
		<th align="right">Incl.</th>
		<th align="left">Aantal</th>
		<th align="left">Nabesteld</th>
		<th align="right">Totaal Excl.</th>
		<th align="right">Totaal Incl.</th>
	</tr>
{% for line in order.lines %}
	<tr>
		<td>{{ line.article_nr }}</td>
		<td>{{ m.rsc[line.rsc_id].title }}</td>
		<td>{{ line.variant|default:"-" }}</td>
		<td align="right">&euro;{{ line.price_excl|format_price }}</td>
		<td align="right">&euro;{{ line.price_incl|format_price }}</td>
		<td>{{ line.quantity }}</td>
		<td>{{ line.backorder }}</td>
		<td align="right">&euro;{{ line.total_price_excl|format_price }}</td>
		<td align="right">&euro;{{ line.total_price_incl|format_price }}</td>
	</tr>
{% endfor %}

<tr>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td align="right"><strong>Totaal</strong></td>
	<td style="border-top: 1px solid #222" align="right">&euro;{{ order.total_price_excl|format_price }}</td>
	<td style="border-top: 1px solid #222" align="right">&euro;{{ order.total_price_incl|format_price }}</td>
</tr>

{#
<tr>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td>&nbsp;</td>
	<td colspan="2" align="right">
		Betaald met
		{% ifequal order.payment_method "card" %}Credit Card
		{% else %}
			{% ifequal order.payment_method "ideal" %}iDeal
			{% else %}
				{% ifequal order.payment_method "paypal" %}PayPal
				{% else %}
					{{ order.payment_method }}
				{% endifequal %}
			{% endifequal %}
		{% endifequal %}
	</td>
</tr>
#}

</table>

{#
{% print order %}
#}
