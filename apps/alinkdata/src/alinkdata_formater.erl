-module(alinkdata_formater).

%% API
-export([format_field/4, format_type/1]).



format_field(Table, Field, Value, Acc) ->
    Acc#{get_field_name(Table, Field) => Value}.

get_field_name(_, <<"create_by">>) ->
    <<"createBy">>;
get_field_name(_, <<"create_time">>) ->
    <<"createTime">>;
get_field_name(_, <<"update_by">>) ->
    <<"updateBy">>;
get_field_name(_, <<"update_time">>) ->
    <<"updateTime">>;
get_field_name(_, <<"del_flag">>) ->
    <<"delFlag">>;
get_field_name(_, <<"dept_id">>) ->
    <<"deptId">>;
get_field_name(_, <<"parent_id">>) ->
    <<"parentId">>;
get_field_name(_, <<"order_num">>) ->
    <<"orderNum">>;
get_field_name(_, <<"dept_name">>) ->
    <<"deptName">>;

get_field_name(<<"menu">>, <<"is_cache">>) ->
    <<"isCache">>;
get_field_name(<<"menu">>, <<"is_frame">>) ->
    <<"isFrame">>;
get_field_name(<<"menu">>, <<"menu_id">>) ->
    <<"menuId">>;
get_field_name(<<"menu">>, <<"menu_name">>) ->
    <<"menuName">>;
get_field_name(<<"menu">>, <<"menu_type">>) ->
    <<"menuType">>;

get_field_name(<<"dict_data">>, <<"css_class">>) ->
    <<"cssClass">>;
get_field_name(<<"dict_data">>, <<"dict_code">>) ->
    <<"dictCode">>;
get_field_name(<<"dict_data">>, <<"dict_label">>) ->
    <<"dictLabel">>;
get_field_name(<<"dict_data">>, <<"dict_sort">>) ->
    <<"dictSort">>;
get_field_name(<<"dict_data">>, <<"dict_type">>) ->
    <<"dictType">>;
get_field_name(<<"dict_data">>, <<"dict_value">>) ->
    <<"dictValue">>;
get_field_name(<<"dict_data">>, <<"is_default">>) ->
    <<"isDefault">>;
get_field_name(<<"dict_data">>, <<"list_class">>) ->
    <<"listClass">>;

get_field_name(<<"user">>, <<"login_date">>) ->
    <<"loginDate">>;
get_field_name(<<"user">>, <<"login_ip">>) ->
    <<"loginIp">>;
get_field_name(<<"user">>, <<"nick_name">>) ->
    <<"nickName">>;
get_field_name(<<"user">>, <<"user_id">>) ->
    <<"userId">>;
get_field_name(<<"user">>, <<"user_name">>) ->
    <<"userName">>;
get_field_name(<<"user">>, <<"user_type">>) ->
    <<"userType">>;

get_field_name(<<"role">>, <<"data_scope">>) ->
    <<"dataScope">>;
get_field_name(<<"role">>, <<"dept_check_strictly">>) ->
    <<"deptCheckStrictly">>;
get_field_name(<<"role">>, <<"menu_check_strictly">>) ->
    <<"menuCheckStrictly">>;
get_field_name(<<"role">>, <<"role_id">>) ->
    <<"roleId">>;
get_field_name(<<"role">>, <<"role_key">>) ->
    <<"roleKey">>;
get_field_name(<<"role">>, <<"role_name">>) ->
    <<"roleName">>;
get_field_name(<<"role">>, <<"role_sort">>) ->
    <<"roleSort">>;

get_field_name(<<"post">>, <<"post_code">>) ->
    <<"postCode">>;
get_field_name(<<"post">>, <<"post_id">>) ->
    <<"postId">>;
get_field_name(<<"post">>, <<"post_name">>) ->
    <<"postName">>;
get_field_name(<<"post">>, <<"post_sort">>) ->
    <<"postSort">>;

get_field_name(<<"config">>, <<"config_id">>) ->
    <<"configId">>;
get_field_name(<<"config">>, <<"config_key">>) ->
    <<"configKey">>;
get_field_name(<<"config">>, <<"config_name">>) ->
    <<"configName">>;
get_field_name(<<"config">>, <<"config_type">>) ->
    <<"configType">>;
get_field_name(<<"config">>, <<"config_value">>) ->
    <<"configValue">>;

get_field_name(<<"dict_type">>, <<"dict_id">>) ->
    <<"dictId">>;
get_field_name(<<"dict_type">>, <<"dict_name">>) ->
    <<"dictName">>;
get_field_name(<<"dict_type">>, <<"dict_type">>) ->
    <<"dictType">>;

get_field_name(<<"notice">>, <<"notice_content">>) ->
    <<"noticeContent">>;
get_field_name(<<"notice">>, <<"notice_id">>) ->
    <<"noticeId">>;
get_field_name(<<"notice">>, <<"notice_title">>) ->
    <<"noticeTitle">>;
get_field_name(<<"notice">>, <<"notice_type">>) ->
    <<"noticeType">>;

get_field_name(<<"job">>, <<"cron_expression">>) ->
    <<"cronExpression">>;
get_field_name(<<"job">>, <<"invoke_target">>) ->
    <<"invokeTarget">>;
get_field_name(<<"job">>, <<"job_group">>) ->
    <<"jobGroup">>;
get_field_name(<<"job">>, <<"job_id">>) ->
    <<"jobId">>;
get_field_name(<<"job">>, <<"job_name">>) ->
    <<"jobName">>;
get_field_name(<<"job">>, <<"misfire_policy">>) ->
    <<"misfirePolicy">>;


get_field_name(<<"gen_table">>, <<"tpl_category">>) ->
    <<"tplCategory">>;
get_field_name(<<"gen_table">>, <<"table_name">>) ->
    <<"tableName">>;
get_field_name(<<"gen_table">>, <<"table_id">>) ->
    <<"tableId">>;
get_field_name(<<"gen_table">>, <<"table_comment">>) ->
    <<"tableComment">>;
get_field_name(<<"gen_table">>, <<"sub_table_name">>) ->
    <<"subTableName">>;
get_field_name(<<"gen_table">>, <<"sub_table_fk_name">>) ->
    <<"subTableFkName">>;
get_field_name(<<"gen_table">>, <<"package_name">>) ->
    <<"packageName">>;
get_field_name(<<"gen_table">>, <<"module_name">>) ->
    <<"modulenName">>;
get_field_name(<<"gen_table">>, <<"gen_type">>) ->
    <<"genType">>;
get_field_name(<<"gen_table">>, <<"gen_path">>) ->
    <<"genPath">>;
get_field_name(<<"gen_table">>, <<"function_name">>) ->
    <<"functionName">>;
get_field_name(<<"gen_table">>, <<"function_author">>) ->
    <<"functionAuthor">>;
get_field_name(<<"gen_table">>, <<"class_name">>) ->
    <<"className">>;
get_field_name(<<"gen_table">>, <<"business_name">>) ->
    <<"businessName">>;



get_field_name(<<"gen_table_column">>, <<"column_comment">>) ->
    <<"columnComment">>;
get_field_name(<<"gen_table_column">>, <<"column_id">>) ->
    <<"columnId">>;
get_field_name(<<"gen_table_column">>, <<"column_name">>) ->
    <<"columnName">>;
get_field_name(<<"gen_table_column">>, <<"column_type">>) ->
    <<"columnType">>;
get_field_name(<<"gen_table_column">>, <<"dict_type">>) ->
    <<"dictType">>;
get_field_name(<<"gen_table_column">>, <<"html_type">>) ->
    <<"htmlType">>;
get_field_name(<<"gen_table_column">>, <<"is_edit">>) ->
    <<"isEdit">>;
get_field_name(<<"gen_table_column">>, <<"is_increment">>) ->
    <<"isIncrement">>;
get_field_name(<<"gen_table_column">>, <<"is_insert">>) ->
    <<"isInsert">>;
get_field_name(<<"gen_table_column">>, <<"is_list">>) ->
    <<"isList">>;
get_field_name(<<"gen_table_column">>, <<"is_pk">>) ->
    <<"isPk">>;
get_field_name(<<"gen_table_column">>, <<"is_query">>) ->
    <<"isQuery">>;
get_field_name(<<"gen_table_column">>, <<"is_required">>) ->
    <<"isRequired">>;
get_field_name(<<"gen_table_column">>, <<"java_field">>) ->
    <<"javaField">>;
get_field_name(<<"gen_table_column">>, <<"java_type">>) ->
    <<"javaType">>;
get_field_name(<<"gen_table_column">>, <<"query_type">>) ->
    <<"queryType">>;
get_field_name(<<"gen_table_column">>, <<"table_id">>) ->
    <<"tableId">>;

get_field_name(_, Field) -> Field.


format_type(<<"datetime">>) ->
    <<"string">>;
format_type(<<"varchar", _/binary>>) ->
    <<"string">>;
format_type(<<"char", _/binary>>) ->
    <<"string">>;
format_type(<<"int", _/binary>>) ->
    <<"integer">>;
format_type(<<"longblob">>) ->
    <<"string">>;
format_type(<<"bigint", _/binary>>) ->
    <<"integer">>;
format_type(<<"tinyint", _/binary>>) ->
    <<"integer">>;
format_type(Type) ->
    Type.
