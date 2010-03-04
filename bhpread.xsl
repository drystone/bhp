<?xml version="1.0"?>

<!--

Copyright 2010 John Hedges

This file is part of bhp.

bhp is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

bhp is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
bhp. If not, see [http://www.gnu.org/licenses/].

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="xml" indent="yes"/>

	<xsl:template match="temperatures">
		<temperatures>
			<xsl:apply-templates select="temperature"/>
		</temperatures>
	</xsl:template>

	<xsl:template match="temperature">
		<xsl:variable name="thermometer" select="document('thermometers.xml')/thermometers/thermometer[@onewire-id=current()/@onewire-id]"/>
		<temperature onewire-id="{@onewire-id}">
			<xsl:if test="$thermometer/@id">
				<xsl:attribute name="thermometer-id"><xsl:value-of select="$thermometer/@id"/></xsl:attribute>
			</xsl:if>
			<xsl:choose>
				<xsl:when test="$thermometer/@correction">
					<xsl:value-of select=".+$thermometer/@correction"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="."/>
				</xsl:otherwise>
			</xsl:choose>
		</temperature>
	</xsl:template>

</xsl:stylesheet>
