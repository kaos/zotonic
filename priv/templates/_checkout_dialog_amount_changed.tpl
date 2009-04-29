<p>We konden niet al uw producten alloceren voor de opgegeven prijs. Daardoor is het totaalbedrag van uw order verandert.</p>

<p>Het nieuwe totaalbedrag is &euro;{{ total_amount|format_price }}</p>

<p>Dit is exclusief de eventuele order- en bestelkosten.</p>

{% button text="Akkoord" action={dialog_close} postback={proceed amount=total_amount details=details} %}
{% button text="Annuleer" action={dialog_close} action={redirect back} %}
