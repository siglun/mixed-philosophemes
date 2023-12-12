<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:t="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.w3.org/1999/xhtml" 
    exclude-result-prefixes="t"
    version="1.0">

  <xsl:param name="base_href">https://github.com/siglun/danish-sonnets/blob/main/</xsl:param>
  
<xsl:output method="text"
	    encoding="UTF-8"
	    indent="no"/>

<xsl:strip-space elements="t:p t:list t:item t:ref" />
<!-- xsl:preserve-space elements="t:emph t:author t:title t:ref"/ -->

<xsl:template match="/">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="t:TEI">
<xsl:apply-templates select="t:text"/>
</xsl:template>

<xsl:template match="t:text">
<xsl:apply-templates select="t:front"/>
<xsl:apply-templates select="t:body"/>
<!--.SH
Notes
<xsl:for-each select="//t:note">
.IP <xsl:value-of select="position()"/><xsl:text>
</xsl:text><xsl:apply-templates  mode="generatetext" select="."/>
</xsl:for-each -->
<xsl:apply-templates select="t:back"/>
</xsl:template>

<xsl:template match="t:front">
.TL
<xsl:for-each select="t:docTitle/t:titlePart" >
<xsl:for-each select="t:title">
<xsl:apply-templates/><xsl:if test="position() &lt; last()"><xsl:text>
.br  
</xsl:text></xsl:if></xsl:for-each>
</xsl:for-each>
.AU
<xsl:apply-templates select="t:docAuthor/t:name" />
.AI
<xsl:apply-templates select="t:docAuthor/t:address"/>
.AB
<xsl:apply-templates select="t:div[@type='abstract']"/>
.AE
</xsl:template>

<xsl:template match="t:body">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="t:back">
<xsl:apply-templates/>
</xsl:template>


<xsl:template match="t:div[@type='abstract']">
<xsl:apply-templates/>  
</xsl:template>

<xsl:template match="t:div[@type='abstract']/t:p">
.LP
.vs -2
.ps -2
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="t:div">
<xsl:if test="@xml:id">  
.pdfhref M -N <xsl:value-of select="@xml:id"/><xsl:text>
</xsl:text></xsl:if>  
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="t:listBibl">
<xsl:apply-templates select="t:head"/>
<xsl:apply-templates select="t:bibl">
<xsl:sort select="t:author[1]|t:title[1]" data-type="text"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="t:bibl">
.XP
.pdfhref M -N <xsl:value-of select="@xml:id"/><xsl:text>
</xsl:text><xsl:for-each select="t:author|t:editor"><xsl:if test="position() > 1 and position()=last()"><xsl:text> and </xsl:text></xsl:if><xsl:apply-templates/><xsl:if test="position() >= 1 and not(position() = last())"><xsl:text>, </xsl:text></xsl:if></xsl:for-each><xsl:if test="t:date"><xsl:text>,
</xsl:text><xsl:apply-templates select="t:date"/><xsl:text>. </xsl:text></xsl:if><xsl:if test="t:title">
<xsl:if test="t:title[@level = 'a']">
<xsl:apply-templates select="t:title[@level = 'a']"/><xsl:text>. </xsl:text><xsl:if test="t:title[@level = 'j']|t:title[@level = 'm']"><xsl:text> In:
</xsl:text></xsl:if>
</xsl:if>
<xsl:if test="t:title[@level = 'j']|t:title[@level = 'm']">\fI<xsl:apply-templates select="t:title[@level = 'j']|t:title[@level = 'm']"/>\fP<xsl:text> </xsl:text>
</xsl:if>
</xsl:if>
<xsl:if test="t:biblScope[@type='volume']">
<xsl:text>Vol. </xsl:text><xsl:apply-templates select="t:biblScope[@type='volume']"/><xsl:if test="t:biblScope[@type='number']">(<xsl:apply-templates select="t:biblScope[@type='number']"/>)<xsl:choose><xsl:when test="t:biblScope[@type='pp']"><xsl:text>, </xsl:text></xsl:when><xsl:otherwise><xsl:text>. </xsl:text></xsl:otherwise></xsl:choose></xsl:if></xsl:if> <xsl:if test="t:biblScope[@type='pp']"> <xsl:text>pp. </xsl:text><xsl:apply-templates select="t:biblScope[@type='pp']"/><xsl:text>. </xsl:text></xsl:if>
<xsl:if test="t:note">
<xsl:apply-templates select="t:note/node()"/>
</xsl:if>
<xsl:if test="t:ref"><xsl:text>
.br  
\s-2\f(CR</xsl:text><xsl:apply-templates select="t:ref"/>\fP\s+2
</xsl:if>

</xsl:template>

<xsl:template match="t:note">\**<xsl:text>
.FS
</xsl:text><xsl:apply-templates/><xsl:text>
.FE
</xsl:text></xsl:template>

<xsl:template mode="generatetext" match="t:note">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="t:quote"><xsl:text> \(lq</xsl:text><xsl:apply-templates/><xsl:text>\(rq</xsl:text><xsl:if test="@rend = 'space'"><xsl:text> </xsl:text></xsl:if></xsl:template>

<xsl:template match="t:head">
.SH
<xsl:apply-templates/><xsl:text>
</xsl:text></xsl:template>

<xsl:template match="t:lg">
.IP<xsl:text>
</xsl:text><xsl:if test="@xml:id">  
.pdfhref M -N <xsl:value-of select="@xml:id"/>
</xsl:if>
<xsl:apply-templates/>  
</xsl:template>

<xsl:template match="t:l">
<xsl:apply-templates/>
.br
</xsl:template>

<xsl:template match="t:lb"><xsl:text>
.br
</xsl:text></xsl:template>


<xsl:template match="t:p">
.LP<xsl:text>
</xsl:text><xsl:if test="@xml:id">  
.pdfhref M -N <xsl:value-of select="@xml:id"/><xsl:text>
</xsl:text></xsl:if>  
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="t:ref[contains(substring(@target,1,1),'#')]">
.pdfhref L -D <xsl:value-of select="substring-after(@target,'#')"/> <xsl:text> </xsl:text> <xsl:apply-templates/> <xsl:text>
\&amp;</xsl:text></xsl:template>

<xsl:template match="t:ref">
<xsl:variable name="href">  
<xsl:choose>
<xsl:when test="contains(@target,'http')"><xsl:value-of select="@target"/></xsl:when>
<xsl:otherwise><xsl:value-of select="concat($base_href,@target)"/></xsl:otherwise>
</xsl:choose>
</xsl:variable>
<xsl:choose>
<xsl:when test="string-length(.) &gt; 0"><xsl:text>  
</xsl:text>.pdfhref W -D <xsl:value-of select="$href"/> <xsl:text> </xsl:text> <xsl:apply-templates/>
</xsl:when>
<xsl:otherwise><xsl:text>
</xsl:text>.pdfhref W -D <xsl:value-of select="$href"/> <xsl:text> </xsl:text> <xsl:value-of select="@target"/>
</xsl:otherwise>
</xsl:choose><xsl:text>
</xsl:text></xsl:template>

<xsl:template match="t:list[@type='ordered']">
<xsl:for-each select="t:item">
.IP <xsl:value-of select="position()"/><xsl:text>
</xsl:text><xsl:apply-templates/>
</xsl:for-each>
</xsl:template>

<xsl:template match="t:list">
<xsl:for-each select="t:item">
<xsl:text>
.IP \s+1\(bu\s-1
</xsl:text><xsl:apply-templates/>
</xsl:for-each>
</xsl:template>

<xsl:template match="t:item">
<xsl:apply-templates/>
</xsl:template>
  
<xsl:template match="t:address">
<xsl:for-each select="t:addrLine">
<xsl:apply-templates/><xsl:text>
</xsl:text></xsl:for-each>
</xsl:template>

<xsl:template match="t:figure"><xsl:text>
.KF</xsl:text>
<xsl:if test="@xml:id"><xsl:text>  
.pdfhref M -N </xsl:text> <xsl:value-of select="@xml:id"/><xsl:text>
</xsl:text></xsl:if><xsl:apply-templates/><xsl:text>
.KE
.sp
</xsl:text></xsl:template>

<xsl:template match="t:figure/t:head|t:table/t:head"><xsl:text>
.sp
.QP
.vs -2
</xsl:text>\s-2<xsl:apply-templates/><xsl:text>\s+2
.vs
.sp
</xsl:text></xsl:template>

<xsl:template match="t:graphic"><xsl:if test="@n"><xsl:text>.in 2c
.ps -2
</xsl:text><xsl:value-of select="@n"/><xsl:text>
.in
.ps
</xsl:text>
</xsl:if>
<xsl:text>
.PDFPIC </xsl:text><xsl:value-of select="concat(substring-before(substring-after(@url,'main/'),'.'),'.pdf')"/><xsl:text> 12.0c 7.2c</xsl:text> <!-- xsl:value-of select="substring-before(@width,'m')"/ --><xsl:text>
</xsl:text></xsl:template>
  

<xsl:template match="t:emph[@rend='bold']"> \fB<xsl:apply-templates/>\fP</xsl:template>

<xsl:template match="t:p/t:title"><xsl:text> \fI</xsl:text><xsl:apply-templates/><xsl:text>\fP</xsl:text></xsl:template>

<xsl:template match="t:hi[@rend='italic']|t:hi[@rend='italics']"> \fI<xsl:apply-templates/>\fP</xsl:template>
<xsl:template match="t:hi[@rend='bold']"> \fB<xsl:apply-templates/>\fP</xsl:template>
<xsl:template match="t:hi[@rend='monospaced']"> \f(CR\s-1<xsl:apply-templates/>\s+1\fP</xsl:template>


<xsl:template match="t:eg[@xml:space='preserve']">
.DS L
\f(CR\s-2<xsl:value-of   select="."/>\fP
.DE
</xsl:template>

<xsl:template match="t:eg"><xsl:text>\&amp; </xsl:text><xsl:value-of disable-output-escaping="yes"  select="concat('\f(CR',normalize-space(.),'\fP')"/><xsl:text> </xsl:text></xsl:template>

<xsl:template match="t:table[@rendition]">
<xsl:variable name="rendition" select="@rendition"/>
<xsl:text>
.KF</xsl:text>
<xsl:if test="@xml:id"><xsl:text>
.pdfhref M -N </xsl:text> <xsl:value-of select="@xml:id"/><xsl:text>
</xsl:text></xsl:if>
<xsl:apply-templates select="t:head"/>
.TS
<xsl:value-of select="//t:tagsDecl/t:rendition[@xml:id = $rendition]"/><xsl:for-each select="t:row[@role='label']/t:cell"><xsl:text>T{
.ps -2
</xsl:text><xsl:apply-templates/>
.ps +2<xsl:text>
T}</xsl:text><xsl:choose><xsl:when test="position() &lt; last()">;</xsl:when><xsl:otherwise><xsl:text>
</xsl:text></xsl:otherwise></xsl:choose></xsl:for-each>

<xsl:for-each select="t:row[@role='data']">
<xsl:for-each select="t:cell"><xsl:choose><xsl:when test="@rend='text'"><xsl:text>T{
.na
</xsl:text><xsl:if test="@xml:id">.pdfhref M -N <xsl:value-of select="@xml:id"/></xsl:if>
\s-2<xsl:apply-templates/>\s+2
<xsl:text>
T}</xsl:text></xsl:when><xsl:otherwise><xsl:apply-templates/></xsl:otherwise></xsl:choose><xsl:choose><xsl:when test="position() &lt; last()">;</xsl:when><xsl:otherwise><xsl:text>
</xsl:text></xsl:otherwise></xsl:choose></xsl:for-each>
</xsl:for-each>
.TE
.sp
.KE
</xsl:template>



<xsl:template match="t:table"><xsl:text>
.KF</xsl:text>
<xsl:if test="@xml:id"><xsl:text>
.pdfhref M -N </xsl:text> <xsl:value-of select="@xml:id"/><xsl:text>
</xsl:text></xsl:if>
<xsl:apply-templates select="t:head"/>
.TS
tab(;) expand ;
<xsl:for-each select="t:row[@role='label']/t:cell">lb </xsl:for-each>;
<xsl:for-each select="t:row[@role='label']/t:cell">l </xsl:for-each>.
<xsl:for-each select="t:row[@role='label']/t:cell"><xsl:text>T{
</xsl:text>\s-2<xsl:apply-templates/>\s+2<xsl:text>
T}</xsl:text><xsl:choose><xsl:when test="position() &lt; last()">;</xsl:when><xsl:otherwise><xsl:text>
</xsl:text></xsl:otherwise></xsl:choose></xsl:for-each>
_
<xsl:for-each select="t:row[@role='data']">
<xsl:if test="@xml:id">  
.pdfhref M -N <xsl:value-of select="@xml:id"/>
</xsl:if>
<xsl:for-each select="t:cell"><xsl:text>T{
.na
</xsl:text>\s-2<xsl:apply-templates/>\s+2<xsl:text>
T}</xsl:text><xsl:choose><xsl:when test="position() &lt; last()">;</xsl:when><xsl:otherwise><xsl:text>
</xsl:text></xsl:otherwise></xsl:choose></xsl:for-each>
</xsl:for-each>
.TE
.sp
.KE
</xsl:template>

<xsl:template mode="preserve"  match="text()">
<xsl:value-of select="."/>
</xsl:template>

<xsl:template match="text()">
<xsl:value-of select="normalize-space(.)"/>
</xsl:template>

</xsl:stylesheet>
