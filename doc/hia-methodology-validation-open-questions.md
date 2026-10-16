# CREAHIA 健康影響評估方法學開放問題

## 文件用途

CREAHIA 正在把分散在舊參數、舊表格與程式條件中的濃度反應函數，整理成可查詢、可驗證的登錄表。第一階段的工程測試回答的是：在科學設定不變時，新流程能否重現舊流程的數值與可觀察行為。

數值重現不等於科學方法已獲得重新核准。本文件集中記錄程式碼與原始文獻無法自行回答、需要健康影響評估同事判斷的問題。這些問題不阻擋第一階段的內部相容性工作；尚未獲得方法學確認的定義會保持為「僅供歷史重現」，不會被包裝成推薦方法。

這是儲存證據與決策結果的唯一 repository 主檔。同事日常閱讀與回覆使用[同一份 Google Doc](https://docs.google.com/document/d/1gyoCt8tGAapjLY79xwcQvX2MwRYICS1QxTxFUK5PbHw/edit)；確認後的結論再同步回本檔。

## 如何回覆

不需要閱讀 R 程式碼，也不需要一次回答全部問題。請優先查看最接近自己專長的題目，也可以只回覆第一優先問題。

每題都有可複選的方框。請勾選一個或多個最接近的處理方向；若不同資料列或結果需要不同處理，請在「補充意見」註明。若暫時無法判斷，勾選「證據不足」即可。

若核准的修正會改變歷史輸出，實作時必須記錄原因，並在數值指紋比較中列為經授權的差異，不能靜默改變結果。

## 建議審查順序

### 第一優先：已在實際計算中出現

- Q08：糖尿病 PAF 的低、中、高估計順序異常。
- Q09：韓國 C40 計算因缺少年齡權重而省略兒童下呼吸道感染結果。
- Q11：合計疾病與細分疾病的組成及重複計算規則。

### 第二優先：可能直接改變健康結果的意義

- Q02：以氣喘新發風險估計計算氣喘盛行率。
- Q05–Q07：死亡研究與舊疾病標籤、YLL 應用之間的不一致。
- Q10：Deaths、YLL、YLD 是否及何時可共用同一條風險曲線。

### 第三優先：可依專長逐步審查

- Q01、Q03、Q04，以及與第二階段正式預設有關的 Q12。

## 問題總覽

| 編號 | 主題 | 最需要確認的事情 | 目前影響 |
| --- | --- | --- | --- |
| Q01 | 氣喘急診就醫的年齡與效應值 | 年齡欄位代表研究族群、實際計算人口，還是輸出標籤？ | 影響歷史定義能否成為來源一致的方法 |
| Q02 | NO2 氣喘盛行率 | 新發風險估計能否用於盛行率？ | 可能需要淘汰或更換來源 |
| Q03 | 早產與低出生體重 | 勝算比、情境值與足月低出生體重應如何表示？ | 影響效果量及不確定性解讀 |
| Q04 | 工作缺勤 | 工作年齡、暴露時間尺度及活動受限日如何處理？ | 影響適用人口與重複計算 |
| Q05 | 長期 O3 死亡 | 呼吸系統死亡風險能否套用到 COPD 死亡與 YLL？ | 影響疾病結果的定義 |
| Q06 | 長期 NO2 死亡及 PM2.5 重疊 | 全自然死因如何對應 NCD.LRI、YLL 與重疊調整？ | 可能漏算或重複計算 |
| Q07 | 長期 SO2 死亡 | 全死因研究如何對應 NCD.LRI、YLL、反事實濃度與單位？ | 影響方法是否來源一致 |
| Q08 | 糖尿病 PAF 不確定範圍 | 低、中、高估計為何順序或符號異常？ | 已在新舊流程中產生警告 |
| Q09 | 韓國 C40 兒童 LRI | 應使用哪套年齡權重，或正式標示不提供？ | 實際輸出會少一項結果 |
| Q10 | Deaths、YLL、YLD 共用曲線 | 什麼證據足以將一種結果的風險曲線套用到另一種結果？ | 為 Q05–Q07 建立一致原則 |
| Q11 | 合計疾病與重複計算 | TotResp、TotCV、AllCause 各自包含什麼？ | 影響多個 preset／Set 的加總 |
| Q12 | 未來推薦組合與正式預設 | 哪些歷史組合只能重現，哪些可成為推薦方法？ | 影響第二階段公開介面，不要求本次會議定案 |

## Q01 氣喘急診就醫：年齡標籤與研究族群是否一致？

### 已確認的證據

- 舊 CRF 表以 `Asthma.0to17` 與 `Asthma.18to99` 作為原因名稱的一部分。這些是歷史標籤，不能單靠名稱證明研究族群真的涵蓋 0–17 歲或 18–99 歲。
- 輸出端另有年齡轉換：名稱含 `0to17` 或 `1to18` 時顯示為 0–18；其他氣喘列通常顯示為 25+。因此，資料列標籤、研究族群與輸出年齡目前不是同一件事。

| 歷史列 | 舊資料中的內容 | 研究證據 | 尚未釐清 |
| --- | --- | --- | --- |
| L03 | PM2.5；`Asthma.0to17`；1.025（1.013–1.037） | 可對應 Zheng 2015 的兒童子群 | 研究中的兒童是否等同 0–17，或輸出中的 0–18？ |
| L04 | PM2.5；`Asthma.18to99`；1.023（1.015–1.031） | 對應全年齡合併估計，不是成人子群 | 為何全年齡估計被用於成人標籤？ |
| L05 | O3；`Asthma.0to17`；1.018（1.010–1.024） | 中央估計可對應全年齡 O3 結果 | 兒童標籤的低、高估計來源尚未追溯 |
| L06 | O3；`Asthma.18to99`；1.018（1.012–1.022） | 對應全年齡合併估計 | 同樣不是成人子群估計 |

深入查核：[舊 CRF 表 L03–L06](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/rr/processed/CRFs.csv#L4-L7)、[整理後的歷史登錄資料](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L4-L7)、[Zheng 2015 來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L8)、[輸出年齡轉換程式](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/R/compute_hia_utils.R#L234-L239)。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若 L03–L06 需要分別處理，請在「補充意見」註明。

- [ ] A. 只在研究族群與計算族群一致時，才允許作為正式方法。
- [ ] B. 保留歷史數值以重現舊結果，但標為「僅供歷史相容」，不作為新的方法學預設。
- [ ] C. 分開記錄「研究族群」、「實際計算族群」與「輸出年齡標籤」，再由方法學規則決定能否使用。
- [ ] D. 目前證據不足，先暫停這四列中的部分或全部。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

**主要來源：** [Zheng et al. 2015](https://doi.org/10.1371/journal.pone.0138146)

## Q02 NO2 氣喘盛行率：能否沿用新發風險？

### 一句話說明

舊資料用同一組效應值計算氣喘新發與氣喘盛行，但目前找到的研究只支持新發或發展氣喘。

### 目前證據

- 歷史 L01（新發）與 L02（盛行）使用完全相同的風險值、濃度增量和反事實濃度。
- Khreis 2017 研究的是兒童氣喘發展風險；Achakulwisut 2019 將它用於 1–18 歲兒童氣喘新發。
- 目前沒有找到支持把同一效應值直接套用到氣喘盛行率的來源。

### 這代表什麼

新發率回答一段期間內出現多少新病例；盛行率回答某時點或期間共有多少既有病例。兩者不是只換名稱即可互用的疾病率指標，因此 L02 的歷史數值可重現，不等於它已獲得方法學支持。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 將未來方法改為來源支持的兒童氣喘新發結果。
- [ ] B. 提供盛行率專屬研究與完整計算方法後，重新建立盛行率定義。
- [ ] C. 保留 L02 作歷史重現，但不列為推薦方法。
- [ ] D. 目前證據不足，先暫停 L02。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看歷史 L01 與 L02 的參數](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L2-L3)
- [查看兩篇文獻的來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L7-L8)
- [查看 Khreis 等人 2017 原始研究](https://doi.org/10.1016/j.envint.2016.11.012)
- [查看 Achakulwisut 等人 2019 原始研究](https://doi.org/10.1016/S2542-5196(19)30046-4)

## Q03 早產與低出生體重：效果量與結果定義

### 一句話說明

早產與低出生體重的舊計算把勝算比當成相對風險使用，而且早產的低、高值其實是情境值，不是信賴區間。

### 目前證據

- 早產來源的基準勝算比為 1.15；1.07 與 1.16 是低、高情境輸入，不是 95% 信賴區間。
- 低出生體重來源報告的是足月低出生體重，舊結果名稱沒有完整保留「足月」限制。
- 兩列都把研究報告的勝算比放進對數線性的相對風險計算。

### 這代表什麼

勝算比在結果罕見時可能接近相對風險，但不是天然相同。若不說明近似條件，使用者可能把情境範圍誤解成統計不確定性，也可能把足月低出生體重誤解成所有低出生體重。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 核准把勝算比近似為相對風險，並明確記錄適用條件與限制。
- [ ] B. 使用基線風險把勝算比轉換為相對風險。
- [ ] C. 更換為直接報告相對風險且結果定義相符的研究。
- [ ] D. 保留舊數值作歷史重現，不作為推薦方法。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看舊早產與低出生體重參數](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/rr/processed/CRFs.csv#L8-L9)
- [查看整理後的 L07 與 L08](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L8-L9)
- [查看兩篇文獻的來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L11-L12)
- [查看 Trasande 等人 2016 原始研究](https://doi.org/10.1289/ehp.1510810)
- [查看 Dadvand 等人 2013 原始研究](https://doi.org/10.1289/ehp.1205575)

## Q04 工作缺勤：人口、時間尺度與重複計算

### 一句話說明

HRAPIE 的工作缺勤方法適用於 20–65 歲工作人口與短期 PM2.5 暴露，但舊流程使用不同人口近似，也尚未扣除與活動受限日重疊的天數。

### 目前證據

- HRAPIE 指定 20–65 歲工作人口，並把兩週平均暴露換算為年平均 PM2.5 的短期效應。
- 舊基線用 15–64 歲人口乘上 15 歲以上勞動參與率近似；輸出與預測路徑的年齡定義也不完全相同。
- HRAPIE 要求在加總負擔前，從活動受限日扣除工作缺勤日；舊流程尚未實作。

### 這代表什麼

若人口範圍、暴露時間尺度或扣除規則不同，結果就不再是同一個健康結果。未扣除重疊天數還可能在健康負擔或經濟估值中重複計算。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 採用來源一致的 20–65 歲短期暴露流程，並實作活動受限日扣除規則。
- [ ] B. 指定另一套有證據支持的工作人口、時間尺度及扣除規則。
- [ ] C. 保留舊工作缺勤流程作歷史重現。
- [ ] D. 目前證據不足，先暫停作為推薦方法。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看歷史工作缺勤列 L09](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L10)
- [查看 HRAPIE 來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L13)
- [查看 WHO HRAPIE 原始報告](https://iris.who.int/handle/10665/329636)

## Q05 長期 O3 死亡：呼吸系統死亡能否代表 COPD？

### 一句話說明

來源支持成人呼吸系統死亡，但舊流程把同一效應值套用到 COPD 死亡與 COPD 的損失壽命年。

### 目前證據

- Turner 2016 報告長期 O3 與呼吸系統死亡的效應值，不是 COPD 專屬效應值。
- Malley 2017 使用該效應值估計 30 歲以上呼吸系統死亡，並另外估算 COPD 約占 O3 可歸因呼吸系統死亡的 70–71%。
- 歷史 L10 與 L11 將效應值直接標為 COPD 死亡與損失壽命年，並使用 35 ppb 反事實濃度。

### 這代表什麼

COPD 是呼吸系統死亡的一部分，但「占多數」不代表可直接把呼吸系統效應值當成 COPD 專屬效應值。死亡人數的效應值能否延伸到損失壽命年，也需要另行確認。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 建立來源一致的 30 歲以上呼吸系統死亡方法。
- [ ] B. 提供 COPD 專屬證據後建立 COPD 方法。
- [ ] C. 核准 COPD 或損失壽命年的推導，但明確記錄假設、年齡與反事實濃度。
- [ ] D. 舊 COPD／損失壽命年對應只保留作歷史重現。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看歷史 L10 與 L11](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L11-L12)
- [查看 Turner 與 Malley 的來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L15-L16)
- [查看 Turner 等人 2016 原始研究](https://doi.org/10.1164/rccm.201508-1633OC)
- [查看 Malley 等人 2017 原始研究](https://doi.org/10.1289/EHP1390)

## Q06 長期 NO2 死亡：疾病定義與 PM2.5 重疊

### 一句話說明

HRAPIE 支持 30 歲以上全自然死因死亡，但舊預設把它映射到 NCD.LRI 與損失壽命年，並使用尚待確認的 PM2.5 重疊調整。

### 目前證據

- HRAPIE 的長期 NO2 方法是 30 歲以上全自然死因死亡，並要求考慮與 PM2.5 死亡負擔的重疊。
- 歷史預設 L12–L13 使用 NCD.LRI 死亡與損失壽命年，以及 20 µg/m³ 門檻和三分之一調整。
- C40／南非的 L16–L21 另使用全死因、心血管與呼吸系統結果；其年齡、反事實濃度、損失壽命年與最終重疊規則仍是歷史設定。

### 這代表什麼

只標記「不重複計算」不能說明實際如何處理 NO2 與 PM2.5 的共同風險。疾病定義、年齡、門檻、損失壽命年和重疊公式必須一起確認，否則可能漏算或重複計算。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 採用 30 歲以上全自然死因定義，並明確記錄與 PM2.5 的重疊規則。
- [ ] B. 核准具明確理由的 NCD.LRI 或損失壽命年對應。
- [ ] C. 保留歷史三分之一調整，但只作歷史重現。
- [ ] D. 目前證據不足，先暫停相關列作為推薦方法。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看歷史預設 L12–L13](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L13-L14)
- [查看 C40／南非 L16–L21](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L17-L22)
- [查看 HRAPIE 來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L13)
- [查看 WHO HRAPIE 原始報告](https://iris.who.int/handle/10665/329636)

## Q07 長期 SO2 死亡：全死因如何對應舊結果？

### 一句話說明

Krewski 2009 支持成人全死因死亡，但舊流程另外加入 NCD.LRI、損失壽命年、25–99 歲、近零反事實濃度與單位換算。

### 目前證據

- 研究中的 SO2 效應值為成人全死因死亡，研究族群為 30 歲以上。
- 歷史預設 L14–L15 使用 NCD.LRI 死亡與損失壽命年；C40／南非 L22–L23 使用全死因死亡與損失壽命年。
- 研究本身沒有直接定義舊流程的 25–99 歲、0.02 ppb 反事實濃度、損失壽命年應用或濃度單位乘數。

### 這代表什麼

效應值能對上原始研究，只能確認其中一部分。其餘疾病結果、年齡、反事實濃度和單位轉換仍是獨立的方法學決定。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 建立來源一致的 30 歲以上全死因方法。
- [ ] B. 核准特定歷史轉換規則，並逐項記錄理由。
- [ ] C. 舊 NCD.LRI／損失壽命年對應只保留作歷史重現。
- [ ] D. 目前證據不足，先暫停相關列作為推薦方法。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看歷史預設 L14–L15](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L15-L16)
- [查看 C40／南非 L22–L25](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L23-L26)
- [查看 Krewski 來源說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/references.csv#L14)
- [查看 Krewski 等人 2009 原始報告](https://www.healtheffects.org/system/files/Krewski140.pdf)

## Q08 糖尿病 PAF：低、中、高估計為何異常？

### 一句話說明

歷史糖尿病計算會警告低、中、高可歸因比例沒有正確排序或符號不一致；新舊流程重現同一警告，只證明相容，不代表方法正確。

### 目前證據

- 糖尿病死亡與傷病損失健康年都會產生相同的排序或符號警告。
- Registry 路徑與舊路徑的可歸因比例、最終結果和警告完全一致。
- 異常可能來自來源風險上下界、曲線插值、可歸因比例轉換，或保護性效應的符號處理，目前尚未定位。

### 這代表什麼

相同結果表示 Phase 1 沒有偷偷改變歷史行為；但低、中、高順序異常會影響不確定範圍的解讀，因此不能把相容性測試當成科學核准。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 追溯來源與每一步轉換，修正後記錄經授權的數值差異。
- [ ] B. 確認目前順序具有方法學依據，並補上清楚說明。
- [ ] C. 改用新的來源或風險曲線，舊結果只保留作重現。
- [ ] D. 在釐清前暫停糖尿病結果作為推薦輸出。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看測試中固定的兩則糖尿病警告](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/tests/testthat/test_historical_original_outcome_parity.R#L16-L31)
- [查看新舊 PAF 與結果相同的測試](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/tests/testthat/test_historical_original_outcome_parity.R#L34-L62)
- [查看現行 PAF 不確定性處理說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/doc/paf_uncertainty.md#L27-L28)

## Q09 韓國 C40：兒童 LRI 年齡權重缺失

### 一句話說明

代表性的韓國 C40 計算要求兒童下呼吸道感染死亡，但疾病率資料找不到所需年齡權重，因此程式警告並省略該結果。

### 目前證據

- 方法組合要求 LRI.child 死亡結果。
- C40 疾病率資料沒有該風險曲線需要的年齡權重。
- 新舊流程都省略同一項結果，其餘結果仍可比較；省略不代表健康影響為零。

### 這代表什麼

這是實際輸出缺少一列，而不是只有名稱或文件問題。若沒有明確標示，使用者可能把「無法計算」誤讀為「結果為零」。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 指定一套來源一致的韓國兒童 LRI 年齡權重。
- [ ] B. 經方法學確認後使用另一套可辯護的替代資料。
- [ ] C. 正式標記 C40 不提供這項結果，並在輸出中清楚顯示不可用。
- [ ] D. 目前證據不足，維持警告並禁止把缺值解讀為零。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看缺少年齡權重時的警告與中止邏輯](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/R/crf_apply.R#L160-L185)
- [查看 C40 新舊結果相容性測試](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/tests/testthat/test-historical-c40-crf-parity.R#L1-L43)

## Q10 死亡人數、YLL、YLD 能否共用風險曲線？

### 一句話說明

多個歷史設定把死亡研究的風險曲線套用到損失壽命年，其他設定也可能在死亡人數、損失壽命年與傷病損失健康年之間共用曲線，但目前沒有共同核准原則。

### 目前證據

- Q05–Q07 都包含把死亡研究延伸到損失壽命年的情況。
- 死亡人數、損失壽命年與傷病損失健康年使用不同疾病率與結果意義。
- 現有數值相容性測試只能確認新舊流程一致，不能回答跨結果推導是否有效。

### 這代表什麼

如果沒有共同原則，同一條曲線可能在不同污染物或設定中得到互相矛盾的處理。這題要建立一個可重用的審查標準，而不是逐列猜測。

### 需要你判斷（可複選）

請勾選最接近的處理方式；若不同資料列或結果需要分別處理，請在「補充意見」註明。

- [ ] A. 只有來源直接支持的健康結果才能成為推薦方法。
- [ ] B. 允許 HIA 團隊核准的跨結果推導，但必須記錄推導規則、疾病率與限制。
- [ ] C. 歷史跨結果對應只保留作數值重現。
- [ ] D. 依疾病與研究逐案判斷，不建立通用規則。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看 O3、NO2、SO2 的歷史死亡與 YLL 列](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L11-L16)
- [查看歷史 original-source 的跨結果測試](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/tests/testthat/test_historical_original_outcome_parity.R#L41-L79)
- [查看 HIA 中 PAF 與疾病率相乘的流程](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/doc/hia_overview.md#L103-L116)

## Q11 合計疾病、細分疾病與重複計算

### 一句話說明

合計疾病與細分疾病同時出現時可能重複計算，但 TotResp、TotCV、AllCause 等名稱的實際組成會隨研究改變，不能只靠名稱建立通用規則。

### 目前證據

- NCD.LRI 是非傳染性疾病加下呼吸道感染的合計分類，會與多個細分疾病重疊。
- 目前 dc_groups.csv 記錄 NCD.LRI 與心血管合計的組成關係，選擇衝突測試會阻止已知合計與細分項同時使用。
- 歷史 C40／南非列中的 TotResp、TotCV、AllCause 仍需要依各篇研究確認實際包含哪些疾病。

### 這代表什麼

重複計算規則既不能完全忽略，也不能只靠相似名稱推導。錯誤規則可能把真正不同的結果刪掉，或讓同一健康負擔被加總兩次。

這題需要兩種意見。HIA 專家需要協助確認各篇研究中的合計疾病實際包含哪些細分疾病；套件使用者則可以告訴我們，遇到合計與細分項衝突時，哪一種提示與選擇方式最容易理解。資料結構與程式實作會由維護者決定，不需要使用者設計 API。

### 你認為怎樣的設計在使用套件時最友善？（可複選）

以下設計都以「套件不能默默重複加總」為前提。請勾選一個或多個較符合你工作方式的選項；若不同分析需要不同方式，請在「補充意見」說明使用情境。

- [ ] A. 在選擇階段直接阻止有重複計算風險的組合，列出衝突項目，請使用者改選「合計疾病」或「細分疾病」。這最安全，但需要使用者在計算前做一次選擇。
- [ ] B. 由套件預先採用一套經審查的推薦表示方式，例如預設使用細分疾病；只有進階使用者主動設定時才改用合計疾病。一般操作較簡單，但推薦規則必須清楚且有版本紀錄。
- [ ] C. 允許同一次分析計算合計疾病與細分疾病，但在結果中分開呈現，明確標示「不可相加」，而且不提供包含兩者的總計。方便比較兩種表示方式，但輸出會較複雜。
- [ ] D. 若某個合計疾病的組成尚未獲得確認，就不讓它進入一般推薦方法；仍可在「歷史重現」模式中使用並顯示限制。這最保守，但部分舊分析無法直接轉成新方法。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看目前的疾病合計與細分對照表](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/dc_groups.csv#L1-L12)
- [查看合計與細分衝突測試](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/tests/testthat/test_crf_double_counting.R#L22-L80)
- [查看 C40／南非相關歷史列](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_registry.csv#L17-L26)

## Q12 未來推薦組合與正式預設值

### 一句話說明

歷史 default、C40 與 Krewski–South Africa 都能被重現，但它們是不同來源和工作流程的組合，不能因為技術上可執行就直接宣稱為未來推薦方法。

### 目前證據

- 歷史 default 不是單一科學來源；目前以 historical_default 表示它的相容性角色。
- C40 是專案特定組合；Krewski–South Africa 是國家與工作流程特定組合。
- 風險來源、濃度反應函數組合、疾病率版本與人口年份是四個不同選擇維度。
- 氣喘、出生結果與工作缺勤可作為結果家族的候選組合；死亡組合仍有 Q05–Q11 的方法學問題。

### 這代表什麼

可重現代表我們知道舊流程做了什麼，不代表所有組合都應出現在一般使用者面前。正式預設值會影響沒有主動選擇方法的所有分析，因此需要比歷史相容更高的證據門檻。

### 本階段邊界

本題是第二階段公開介面的重要輸入，但不要求在 Phase 1 說明會形成結論。建議先用真實專案試用，再安排正式方法審查。

這題不是請使用者決定函式名稱或 API 結構，而是想了解日常工作時希望從哪裡開始、願意承擔多少選擇。正式預設與公開介面最後仍由維護者依科學審查、相容性與使用回饋定案。

### 你認為怎樣的設計在使用套件時最友善？（可複選）

請想像你要開始一個新的 HIA 專案，但不想逐列挑選風險曲線。以下哪一種起點最符合你的工作方式？可以複選；若不同專案需要不同方式，請在「補充意見」舉例。

- [ ] A. 提供一個經審查且有版本紀錄的「一般分析推薦組合」；多數使用者可以直接開始，只有需要特殊方法時才修改。操作最簡單，但這個推薦組合的變更會影響許多使用者。
- [ ] B. 依分析目的提供數個清楚命名的推薦組合，例如成人死亡、兒童氣喘或出生結果；使用者先選目的，再由套件帶入相應方法。選擇較容易理解，但跨結果分析可能需要組合多個選項。
- [ ] C. 提供 C40、Krewski–South Africa 等專案／地區工作流程範本，並清楚標示適用情境；沒有一個適用所有人的通用預設。適合重複既有工作流程，但新專案需要判斷最接近哪個範本。
- [ ] D. 不提供自動預設；每次分析都要求使用者明確選擇風險來源、濃度反應函數組合、疾病率版本與人口年份。最透明，但需要較多方法知識與設定時間。
- [ ] E. 其他（請在下方補充意見說明）。

**補充意見：**

### 深入查核（不需要看程式也能回答）

- [查看三個歷史組合的成員](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/compatibility/historical_memberships.csv#L1-L56)
- [查看目前的實驗性預設](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/inst/extdata/crf/presets/experimental_default.csv#L1-L3)
- [查看 Registry 選擇與重複計算說明](https://github.com/energyandcleanair/creahia/blob/4dda2f8ac838c93127d35954a850218d1fe246b6/doc/crf-registry-selection.md#L765-L865)

## 審查後如何處理

每一題最後記錄為下列其中一種結果：

- 核准為來源一致的未來 Registry 方法；
- 僅供歷史重現；
- 以指定來源或定義取代；
- 需要更多證據；
- 部分核准，並明列仍未解決的範圍。

維護者會把確認的決定轉成 Registry 欄位、provenance、選擇規則與測試。尚未決定的項目會保持 provisional，不會用猜測補齊。任何核准後的數值改變，都必須留下原因與經授權的指紋差異。

## 技術追溯資料

- [歷史 CRF migration manifest](../.github/prompts/dev/rr_crf_redesign/08-legacy-crf-migration-manifest.md)：25 個 CRF 定義、55 個 active profile memberships，以及 row-level provenance 與 double-counting 計畫。
- [歷史設定對照](../.github/prompts/dev/rr_crf_redesign/09-historical-config-mapping.md)：default、C40、Krewski–South Africa，以及 RR／EPI／人口年份的完整設定界線。
- [RR/CRF redesign roadmap](../.github/prompts/dev/rr_crf_redesign/00-roadmap.md)：Phase 1 相容性 rollout 與 Phase 2 Registry-first 公開介面的界線。
- `inst/extdata/crf/references.csv`：目前可查詢的文獻資料。
