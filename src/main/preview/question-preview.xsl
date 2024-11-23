<?xml version="1.0" encoding="UTF-8"?>
<!-- question-preview.xsl -->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:output method="html" encoding="UTF-8" indent="yes"/>
    <xsl:param name="css-file" select="'preview.css'"/>

    <!-- Root template -->
    <xsl:template match="/">
        <html>
            <head>
                <meta charset="UTF-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                <title>Question Preview</title>
                <link rel="stylesheet" href="{$css-file}"/>
            </head>
            <body>
                <div class="question-container">
                    <xsl:apply-templates/>
                </div>
            </body>
        </html>
    </xsl:template>

    <!-- Common question template -->
    <xsl:template match="pickQuestion|categoryQuestion">
        <div class="question">
            <xsl:attribute name="class">
                question <xsl:value-of select="local-name()"/>
            </xsl:attribute>
            
            <!-- Common header section -->
            <div class="question-header">
                <span class="question-id">ID: <xsl:value-of select="@id"/></span>
                <span class="question-points">Points: <xsl:value-of select="@points"/></span>
            </div>
            
            <!-- Common metadata -->
            <xsl:apply-templates select="refersToLgs"/>
            
            <!-- Common stem section -->
            <xsl:apply-templates select="stem"/>
            
            <!-- Question-type specific content -->
            <xsl:choose>
                <xsl:when test="local-name()='pickQuestion'">
                    <div class="options-container">
                        <xsl:apply-templates select="pickOptions/option"/>
                    </div>
                </xsl:when>
                <xsl:when test="local-name()='categoryQuestion'">
                    <table class="category-table">
                        <thead>
                            <tr>
                                <xsl:apply-templates select="categoryStatements/categories/category" mode="header"/>
                                <th class="statement-column">Statement</th>
                            </tr>
                        </thead>
                        <tbody>
                            <xsl:apply-templates select="categoryStatements/statements/statement"/>
                        </tbody>
                    </table>
                </xsl:when>
            </xsl:choose>
            
            <!-- Common footer -->
            <xsl:apply-templates select="explanation"/>
        </div>
    </xsl:template>

    <!-- Pick Option template -->
    <xsl:template match="option">
        <div class="option">
            <span class="option-marker">
                <xsl:choose>
                    <xsl:when test="@correct='correct'">
                        <span class="icon-correct">✓</span>
                    </xsl:when>
                    <xsl:when test="@distractor='distractor' or @false='false'">
                        <span class="icon-incorrect">✗</span>
                    </xsl:when>
                </xsl:choose>
            </span>
            <span class="option-identifier"><xsl:value-of select="@identifier"/></span>
            <div class="option-texts">
                <xsl:apply-templates select="text"/>
            </div>
        </div>
    </xsl:template>

    <!-- Category Headers -->
    <xsl:template match="category" mode="header">
        <th class="category-header">
            <xsl:apply-templates select="text"/>
        </th>
    </xsl:template>

    <!-- Category Statement -->
    <xsl:template match="statement">
        <tr class="statement-row">
            <xsl:variable name="correctCat" select="@correctCategory"/>
            <xsl:for-each select="../../categories/category">
                <td class="category-cell">
                    <xsl:if test="@label = $correctCat">
                        <span class="category-marker">✓</span>
                    </xsl:if>
                </td>
            </xsl:for-each>
            <td class="statement-cell">
                <span class="statement-identifier"><xsl:value-of select="@identifier"/>.</span>
                <div class="statement-content">
                    <xsl:apply-templates select="text"/>
                </div>
            </td>
        </tr>
    </xsl:template>

<!-- Learning Goals templates -->
<xsl:template match="refersToLgs">
    <div class="learning-goals">
        <xsl:apply-templates select="lg"/>
    </div>
</xsl:template>

<xsl:template match="lg">
    <span class="learning-goal">
        <!-- Display the learning goal -->
        <xsl:value-of select="@curriculumVersion"/>/<xsl:value-of select="@lg"/>
        
        <!-- Add curriculum links -->
        <span class="curriculum-links">
            <!-- Base URL for curriculum -->
            <xsl:variable name="baseUrl">https://public.isaqb.org/curriculum-foundation/</xsl:variable>
            
            <!-- Version-specific path -->
            <xsl:variable name="versionPath">
                <xsl:if test="starts-with(@curriculumVersion, '2025')">release-candidate/</xsl:if>
            </xsl:variable>
            
            <!-- German link -->
            <a href="{$baseUrl}{$versionPath}curriculum-foundation-de.html#LZ-{@lg}" 
               class="curriculum-link" 
               target="_blank">de</a>
            
            <!-- English link -->
            <a href="{$baseUrl}{$versionPath}curriculum-foundation-en.html#LG-{@lg}" 
               class="curriculum-link" 
               target="_blank">en</a>
        </span>
    </span>
</xsl:template>

    <!-- Common content templates -->
    <xsl:template match="stem">
        <div class="stem">
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>

    <xsl:template match="explanation">
        <div class="explanation">
            <h3>Explanation</h3>
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>

    <xsl:template match="text">
        <div class="lang-text {@xml:lang}">
            <span class="lang-indicator"><xsl:value-of select="@xml:lang"/></span>
            <xsl:value-of select="."/>
        </div>
    </xsl:template>

</xsl:stylesheet>