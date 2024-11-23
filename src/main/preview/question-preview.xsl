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

    <!-- Learning Goals template -->
    <xsl:template match="refersToLgs">
        <div class="learning-goals">
            <xsl:apply-templates select="lg"/>
        </div>
    </xsl:template>

    <xsl:template match="lg">
        <span class="learning-goal">
            <xsl:value-of select="@curriculumVersion"/>/<xsl:value-of select="@lg"/>
        </span>
    </xsl:template>

    <!-- Pick Question template -->
    <xsl:template match="pickQuestion">
        <div class="question pick-question">
            <div class="question-header">
                <span class="question-id">ID: <xsl:value-of select="@id"/></span>
                <span class="question-points">Points: <xsl:value-of select="@points"/></span>
            </div>
            
            <xsl:apply-templates select="refersToLgs"/>
            <xsl:apply-templates select="stem"/>
            
            <div class="options-container">
                <xsl:apply-templates select="pickOptions/option"/>
            </div>
            
            <xsl:apply-templates select="explanation"/>
        </div>
    </xsl:template>

    <!-- Category Question template -->
    <xsl:template match="categoryQuestion">
        <div class="question category-question">
            <div class="question-header">
                <span class="question-id">ID: <xsl:value-of select="@id"/></span>
                <span class="question-points">Points: <xsl:value-of select="@points"/></span>
            </div>
            
            <xsl:apply-templates select="refersToLgs"/>
            <xsl:apply-templates select="stem"/>
            
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

    <!-- Stem template -->
    <xsl:template match="stem">
        <div class="stem">
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>

    <!-- Explanation template -->
    <xsl:template match="explanation">
        <div class="explanation">
            <h3>Explanation</h3>
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>

    <!-- Text template -->
    <xsl:template match="text">
        <div class="lang-text {@xml:lang}">
            <span class="lang-indicator"><xsl:value-of select="@xml:lang"/></span>
            <xsl:value-of select="."/>
        </div>
    </xsl:template>

</xsl:stylesheet>